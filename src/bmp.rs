use std::cmp::Ordering;
use std::convert::TryInto;
use std::fmt::{Debug};
use std::num::NonZeroU32;
use bytemuck::{cast_slice, from_bytes, from_bytes_mut, Pod, Zeroable};
use num_enum::{IntoPrimitive, TryFromPrimitive};
use crate::common::{RGBA, RGBAImage};
use anyhow::{anyhow, bail, Error};








/*
File headers
 */

const BITMAP_FILE_HEADER_SIZE: u32 = 14;
const BITMAP_CORE_HEADER_SIZE: u32 = 12;
const BITMAP_INFO_HEADER_SIZE: u32 = 40;
const BITMAP_V4_HEADER_SIZE: u32 = 108;
const BITMAP_V5_HEADER_SIZE: u32 = 124;
const BITMAP_MAGIC_NUMBER: [u8; 2] = [0x42, 0x4d];

#[derive(Pod, Zeroable, Copy, Clone)]
#[repr(C, packed)]
struct BitmapFileHeader {
    magic_number: [u8; 2],
    file_size: u32,
    reserved1: u16,
    reserved2: u16,
    bitmap_offset: u32
}

#[derive(Pod, Zeroable, Copy, Clone)]
#[repr(C, packed)]
struct BitmapCoreHeader {
    header_size: u32,
    width: u16,
    height: u16,
    planes: u16,
    bit_depth: u16
}

#[derive(Pod, Zeroable, Copy, Clone)]
#[repr(C, packed)]
struct BitmapInfoHeader {
    header_size: u32,
    width: i32,
    height: i32,
    planes: u16,
    bit_depth: u16,
    compression: u32,
    image_size: u32,
    x_pixels_per_meter: i32,
    y_pixels_per_meter: i32,
    colors_used: u32,
    important_colors_used: u32
}

#[derive(Pod, Zeroable, Copy, Clone)]
#[repr(C, packed)]
struct BitmapV4Header {
    header_size: u32,
    width: i32,
    height: i32,
    planes: u16,
    bit_depth: u16,
    compression: u32,
    image_size: u32,
    x_pixels_per_meter: i32,
    y_pixels_per_meter: i32,
    colors_used: u32,
    important_colors_used: u32,
    red_mask: u32,
    green_mask: u32,
    blue_mask: u32,
    alpha_mask: u32,
    color_space_type: u32,
    color_space_red_x: i32,
    color_space_red_y: i32,
    color_space_red_z: i32,
    color_space_green_x: i32,
    color_space_green_y: i32,
    color_space_green_z: i32,
    color_space_blue_x: i32,
    color_space_blue_y: i32,
    color_space_blue_z: i32,
    gamma_red: u32,
    gamma_green: u32,
    gamma_blue: u32
}

#[derive(Pod, Zeroable, Copy, Clone)]
#[repr(C, packed)]
struct BitmapV5Header {
    header_size: u32,
    width: i32,
    height: i32,
    planes: u16,
    bit_depth: u16,
    compression: u32,
    image_size: u32,
    x_pixels_per_meter: i32,
    y_pixels_per_meter: i32,
    colors_used: u32,
    important_colors_used: u32,
    red_mask: u32,
    green_mask: u32,
    blue_mask: u32,
    alpha_mask: u32,
    color_space_type: u32,
    color_space_red_x: i32,
    color_space_red_y: i32,
    color_space_red_z: i32,
    color_space_green_x: i32,
    color_space_green_y: i32,
    color_space_green_z: i32,
    color_space_blue_x: i32,
    color_space_blue_y: i32,
    color_space_blue_z: i32,
    gamma_red: u32,
    gamma_green: u32,
    gamma_blue: u32,
    intent: u32,
    profile_data: u32,
    profile_size: u32,
    reserved: u32
}


/*
Bit depths
 */

#[derive(Debug, IntoPrimitive, TryFromPrimitive, Copy, Clone, Eq, PartialEq)]
#[repr(u16)]
enum BitDepth {
    One = 1,
    Two = 2,
    Four = 4,
    Eight = 8,
    Sixteen = 16,
    TwentyFour = 24,
    ThirtyTwo = 32
}


/*
Compression types
 */

// constants documented in https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-wmf/4e588f70-bd92-4a6f-b77f-35d0feaf7a57
#[derive(Debug, IntoPrimitive, TryFromPrimitive, Copy, Clone, Eq, PartialEq)]
#[repr(u32)]
enum Compression {
    RGB = 0x0,
    RLE8 = 0x1,
    RLE4 = 0x2,
    Bitfields = 0x3

    // The following compression types are documented but unsupported by this decoder as they are
    // vanishingly rare, and quite frankly no other image processing suite seems to support these
    // either.

    // JPEG = 0x4,
    // PNG = 0x5,
    // CMYK = 0xB,
    // CMYKRLE8 = 0xC,
    // CMYKRLE4 = 0xD
}


/*
Each scanline encoding method must implement this trait.
 */

trait EncodingMethod {
    fn stride(&self, width: u64) -> u64;
    fn decode_scanline(&self, index_bytes: &[u8], image_buffer: &mut [RGBA]);
}


/*
Bit depth of 1, RGB compression. Indexes a palette of max size 2 with a single bit.
 */

struct Paletted1<'a> {
    palette: &'a[RGBA]
}

impl EncodingMethod for Paletted1<'_> {
    fn stride(&self, width: u64) -> u64 {
        let data_bytes = width.div_ceil(8);
        data_bytes.div_ceil(4)*4
    }

    fn decode_scanline(&self, index_bytes: &[u8], image_buffer: &mut [RGBA]) {
        // suppose a 0x0 image is read, this ensures we exit normally (not writing anything into these 0-sized buffers
        // rather than panic
        if index_bytes.is_empty() {
            return;
        }

        let mut curr_byte = index_bytes[0]; // immediately overwritten in first iter
        for (i, color) in image_buffer.iter_mut().enumerate() {
            let bit_num = i % 8;
            if bit_num == 0 {
                let byte_num = i / 8;
                curr_byte = index_bytes[byte_num];
            }
            else {
                curr_byte <<= 1;
            }

            // colors are in order from msb to lsb, so we test msb for which color to write
            if curr_byte & 0x80 == 0 {
                *color = self.palette[0];
            }
            else {
                *color = self.palette[1];
            }
        }
    }
}


/*
Bit depth of 2, RGB compression. Indexes a palette of max size 4 with two bits. Non-standard and
generally highly unlikely to be supported.
 */

struct Paletted2<'a> {
    palette: &'a[RGBA]
}

impl EncodingMethod for Paletted2<'_> {
    fn stride(&self, width: u64) -> u64 {
        let data_bytes = (width*2).div_ceil(8);
        data_bytes.div_ceil(4)*4
    }

    fn decode_scanline(&self, index_bytes: &[u8], image_buffer: &mut [RGBA]) {
        // suppose a 0x0 image is read, this ensures we exit normally (not writing anything into these 0-sized buffers
        // rather than panic
        if index_bytes.is_empty() {
            return;
        }

        let mut curr_byte = index_bytes[0]; // immediately overwritten in first iteration
        for (i, color) in image_buffer.iter_mut().enumerate() {
            // from most significant to least significant in a byte,
            // offset should go 6, 4, 2, 0
            let byte_offset = 6 - (i % 4) * 2;
            if byte_offset == 6 {
                let byte_num = i / 4;
                curr_byte = index_bytes[byte_num];
            }

            let index = (curr_byte >> byte_offset) & 0b11;
            *color = self.palette[usize::from(index)];
        }
    }
}


/*
Bit depth of 4, RGB compression. Indexes a palette of max size 16 with 4 bits.
 */

struct Paletted4<'a> {
    palette: &'a[RGBA]
}

impl EncodingMethod for Paletted4<'_> {
    fn stride(&self, width: u64) -> u64 {
        let data_bytes = (width*4).div_ceil(8);
        data_bytes.div_ceil(4)*4
    }

    fn decode_scanline(&self, index_bytes: &[u8], image_buffer: &mut [RGBA]) {
        // suppose a 0x0 image is read, this ensures we exit normally (not writing anything into these 0-sized buffers
        // rather than panic
        if index_bytes.is_empty() {
            return;
        }

        let mut curr_byte = index_bytes[0]; // immediately overwritten in first iteration
        for (i, color) in image_buffer.iter_mut().enumerate() {
            // from most significant to least significant in a byte,
            // offset should go 6, 4, 2, 0
            let is_upper_half = i % 2 == 0;
            if is_upper_half {
                let byte_num = i / 2;
                curr_byte = index_bytes[byte_num];
            }

            let index = if is_upper_half { curr_byte >> 4 } else { curr_byte & 0b1111 };
            *color = self.palette[usize::from(index)];
        }
    }
}


/*
Bit depth of 8, RGB compression. Indexes a palette of max size 256 with 8 bits.
 */

struct Paletted8<'a> {
    palette: &'a[RGBA]
}

impl EncodingMethod for Paletted8<'_> {
    fn stride(&self, width: u64) -> u64 {
        width.div_ceil(4)*4
    }

    fn decode_scanline(&self, index_bytes: &[u8], image_buffer: &mut [RGBA]) {
        for (i, color) in image_buffer.iter_mut().enumerate() {
            *color = self.palette[usize::from(index_bytes[i])];
        }
    }
}


/*
Bit depth of 16, RGB compression. Stores 5 bits for each color channel, with the MSB unused. (no alpha)
 */

struct Uncompressed16 {}

impl EncodingMethod for Uncompressed16 {
    fn stride(&self, width: u64) -> u64 {
        let data_bytes = width*2;
        data_bytes.div_ceil(4)*4
    }

    fn decode_scanline(&self, index_bytes: &[u8], image_buffer: &mut [RGBA]) {
        const RED_MASK: u16 = 0b11111_00000_00000;
        const GREEN_MASK: u16 = 0b00000_11111_00000;
        const BLUE_MASK: u16 = 0b00000_00000_11111;

        // For compatibility, we eschew the more accurate model of
        // color/31*255, but rather shift left by 3 and copy the top 3 bits to the bottom 3. This
        // appears to be the most common/expected way of doing things (e.g., in ffmpeg:
        // https://ffmpeg.org/doxygen/trunk/rgb2rgb_8c_source.html#l00264)
        let word_index_bytes: &[u16] = cast_slice(index_bytes);
        for (i, color) in image_buffer.iter_mut().enumerate() {
            let curr_word = word_index_bytes[i];
            color.r = (((curr_word & RED_MASK) >> 7) | ((curr_word & RED_MASK) >> 12)) as u8;
            color.g = (((curr_word & GREEN_MASK) >> 2) | ((curr_word & GREEN_MASK) >> 7)) as u8;
            color.b = (((curr_word & BLUE_MASK) << 3) | ((curr_word & BLUE_MASK) >> 2)) as u8;
        }
    }
}


/*
Bit depth of 24, RGB compression. Stores a full byte per color channel, with no alpha.
 */

struct Uncompressed24 {}

impl EncodingMethod for Uncompressed24 {
    fn stride(&self, width: u64) -> u64 {
        let data_bytes = width*3;
        data_bytes.div_ceil(4)*4
    }

    fn decode_scanline(&self, index_bytes: &[u8], image_buffer: &mut [RGBA]) {
        let mut index_offset = 0;
        for color in image_buffer {
            color.b = index_bytes[index_offset];
            color.g = index_bytes[index_offset+1];
            color.r = index_bytes[index_offset+2];
            index_offset += 3;
        }
    }
}


/*
Bit depth of 32, RGB compression. Stores a full byte per color channel, with no alpha, leaving a
full byte unused in each set of 4.
 */

struct Uncompressed32 {}

impl EncodingMethod for Uncompressed32 {
    fn stride(&self, width: u64) -> u64 {
        width*4
    }

    fn decode_scanline(&self, index_bytes: &[u8], image_buffer: &mut [RGBA]) {
        let mut index_offset = 0;
        for color in image_buffer {
            color.b = index_bytes[index_offset];
            color.g = index_bytes[index_offset+1];
            color.r = index_bytes[index_offset+2];
            // high byte unused
            index_offset += 4;
        }
    }
}


/*
Bit depth of 16, bitfield compression. Uses header-specified bit masks to decide which bits and how
many bits to assigned to each color channel.
 */

struct Bitfield16 {
    red_mask: u16,
    green_mask: u16,
    blue_mask: u16
}

// We require a constructor for Bitfield16 to check that the input mask fields are contiguous
// and don't overlap (required by https://learn.microsoft.com/en-us/windows/win32/api/wingdi/ns-wingdi-bitmapv5header).
impl Bitfield16 {
    fn new(r: u32, g: u32, b: u32) -> Result<Bitfield16, Error> {
        let Ok(red_mask) = u16::try_from(r) else { bail!("Red bitmask too large for bitfield compression of bitdepth 16") };
        let Ok(green_mask) = u16::try_from(g) else { bail!("Green bitmask too large for bitfield compression of bitdepth 16") };
        let Ok(blue_mask) = u16::try_from(b) else { bail!("Blue bitmask too large for bitfield compression of bitdepth 16") };

        if red_mask.count_ones() != 16 - red_mask.leading_zeros() - red_mask.trailing_zeros() {
            bail!("Red bitmask is not contiguous (mask is {red_mask:#018b})")
        }
        else if green_mask.count_ones() != 16 - green_mask.leading_zeros() - green_mask.trailing_zeros() {
            bail!("Green bitmask is not contiguous (mask is {green_mask:#018b})")
        }
        else if blue_mask.count_ones() != 16 - blue_mask.leading_zeros() - blue_mask.trailing_zeros() {
            bail!("Blue bitmask is not contiguous (mask is {blue_mask:#018b})")
        }
        else if (red_mask | blue_mask | green_mask).count_ones() != red_mask.count_ones() + green_mask.count_ones() + blue_mask.count_ones() {
            bail!("Overlapping bitmasks:\n\tRed is   {red_mask:#018b}\n\tGreen is {green_mask:#018b}\n\tBlue is  {blue_mask:#018b}")
        }

        println!("Bitfield compression masks:\n\tRed is   {red_mask:#018b}\n\tGreen is {green_mask:#018b}\n\tBlue is  {blue_mask:#018b}");
        Ok(Bitfield16 { red_mask, green_mask, blue_mask })
    }

    // For color channels with precision greater than 8, we truncate down to 8. Otherwise, we
    // shift upwards to occupy the most significant bits and repeat the value till all bits are
    // filled. For instance, 0b110 will be converted to 0b11011011.
    fn channel_to_color(channel: u16, mask: u16) -> u8 {
        let mut val = channel & mask;
        val >>= mask.trailing_zeros();
        if mask.count_ones() >= 8 {
            val >>= mask.count_ones() - 8;
        }
        else {
            let mut gaps_left: i32 = 8 - i32::try_from(mask.count_ones()).unwrap();
            val <<= 8 - mask.count_ones();
            let mut base = val;
            while gaps_left > 0 {
                base >>= mask.count_ones();
                val |= base;
                gaps_left -= i32::try_from(mask.count_ones()).unwrap();
            }
        }

        val as u8
    }
}

impl EncodingMethod for Bitfield16 {
    fn stride(&self, width: u64) -> u64 {
        (Uncompressed16 {}).stride(width) // stride is the same as uncompressed 16
    }

    fn decode_scanline(&self, index_bytes: &[u8], image_buffer: &mut [RGBA]) {
        let word_index_bytes: &[u16] = cast_slice(index_bytes);
        for (i, color) in image_buffer.iter_mut().enumerate() {
            let curr_word = word_index_bytes[i];
            color.r = Bitfield16::channel_to_color(curr_word, self.red_mask);
            color.g = Bitfield16::channel_to_color(curr_word, self.green_mask);
            color.b = Bitfield16::channel_to_color(curr_word, self.blue_mask);
        }
    }
}


/*
Bit depth of 32, bitfield compression. Uses header-specified bit masks to decide which bits and how
many bits to assigned to each color channel.
 */


struct Bitfield32 {
    red_mask: u32,
    green_mask: u32,
    blue_mask: u32
}

// We require a constructor for Bitfield16 to check that the input mask fields are contiguous
// and don't overlap (required by https://learn.microsoft.com/en-us/windows/win32/api/wingdi/ns-wingdi-bitmapv5header).
impl Bitfield32 {
    fn new(red_mask: u32, green_mask: u32, blue_mask: u32) -> Result<Bitfield32, Error> {
        if red_mask.count_ones() != 32 - red_mask.leading_zeros() - red_mask.trailing_zeros() {
            bail!("Red bitmask is not contiguous (mask is {red_mask:#034b})")
        }
        else if green_mask.count_ones() != 32 - green_mask.leading_zeros() - green_mask.trailing_zeros() {
            bail!("Green bitmask is not contiguous (mask is {green_mask:#034b})")
        }
        else if blue_mask.count_ones() != 32 - blue_mask.leading_zeros() - blue_mask.trailing_zeros() {
            bail!("Blue bitmask is not contiguous (mask is {blue_mask:#034b})")
        }
        else if (red_mask | blue_mask | green_mask).count_ones() != red_mask.count_ones() + green_mask.count_ones() + blue_mask.count_ones() {
            bail!("Overlapping bitmasks:\n\tRed is   {red_mask:#034b}\n\tGreen is {green_mask:#034b}\n\tBlue is  {blue_mask:#034b}")
        }

        println!("Bitfield compression masks:\n\tRed is   {red_mask:#034b}\n\tGreen is {green_mask:#034b}\n\tBlue is  {blue_mask:#034b}");
        Ok(Bitfield32 { red_mask, green_mask, blue_mask })
    }

    // For color channels with precision greater than 8, we truncate down to 8. Otherwise, we
    // shift upwards to occupy the most significant bits and repeat the value till all bits are
    // filled. For instance, 0b110 will be converted to 0b11011011.
    // This is identical to the methods
    fn channel_to_color(channel: u32, mask: u32) -> u8 {
        let mut val = channel & mask;
        val >>= mask.trailing_zeros();
        if mask.count_ones() >= 8 {
            val >>= mask.count_ones() - 8;
        }
        else {
            let mut gaps_left: i32 = 8 - i32::try_from(mask.count_ones()).unwrap();
            val <<= 8 - mask.count_ones();
            let mut base = val;
            while gaps_left > 0 {
                base >>= mask.count_ones();
                val |= base;
                gaps_left -= i32::try_from(mask.count_ones()).unwrap();
            }
        }
 
        val as u8
    }
}

impl EncodingMethod for Bitfield32 {
    fn stride(&self, width: u64) -> u64 {
        (Uncompressed32 {}).stride(width) // stride is the same as uncompressed 16
    }

    fn decode_scanline(&self, index_bytes: &[u8], image_buffer: &mut [RGBA]) {
        // we need to create a copy here to re-align the index bytes to increments of 4 bytes
        let mut index_bytes_copy = vec![0; index_bytes.len()];
        index_bytes_copy.clone_from_slice(index_bytes);

        let word_index_bytes: &[u32] = cast_slice(index_bytes_copy.as_slice());
        for (i, color) in image_buffer.iter_mut().enumerate() {
            let curr_word = word_index_bytes[i];
            color.r = Bitfield32::channel_to_color(curr_word, self.red_mask);
            color.g = Bitfield32::channel_to_color(curr_word, self.green_mask);
            color.b = Bitfield32::channel_to_color(curr_word, self.blue_mask);
        }
    }
}


/*
Palette parsing
 */
enum PaletteFormat {
    BGRQuad,
    BGRTriple
}

// used for most BMP's
#[derive(Pod, Zeroable, Copy, Clone)]
#[repr(C)]
struct BGRQuad {
    b: u8,
    g: u8,
    r: u8,
    reserved: u8
}

// used by OS2 and OS2v2 BMP's
#[derive(Pod, Zeroable, Copy, Clone)]
#[repr(C)]
struct BGRTriple {
    b: u8,
    g: u8,
    r: u8
}

fn create_palette(buf: &[u8], bit_depth: u16, palette_size: NonZeroU32, palette_format: &PaletteFormat) -> Result<Vec<RGBA>, Error> {
    // rgba_palette is extended to include all possible index values for the given bit depth (2^bit_depth)
    // ensuring that invalid indexes result in a black pixel by default rather than a panic
    let max_palette_size: usize = 1 << bit_depth;
    let mut rgba_palette: Vec<RGBA> = Vec::new();
    rgba_palette.reserve_exact(max_palette_size);
    let index_bytes;

    match palette_format {
        PaletteFormat::BGRQuad => {
            println!("Creating palette from quads...");
            let palette_bytes;
            (palette_bytes, index_bytes) = buf.split_at(palette_size.get().try_into()?);
            let palette: &[BGRQuad] = cast_slice(palette_bytes);
            println!("Calculated palette size of {} ({} colors)", palette_size, palette_size.get()/4);
            rgba_palette.extend(palette.iter().map(|bgr| RGBA { r: bgr.r, g: bgr.g, b: bgr.b, a: 0xFF }));
            rgba_palette.resize(max_palette_size, RGBA { r: 0, g: 0, b: 0, a: 0xFF });
        }
        PaletteFormat::BGRTriple => {
            println!("Creating palette from triples...");
            let palette_bytes;
            (palette_bytes, index_bytes) = buf.split_at(palette_size.get().try_into()?);
            let palette: &[BGRTriple] = cast_slice(palette_bytes);
            println!("Calculated palette size of {} ({} colors)", palette_size, palette_size.get()/3);
            rgba_palette.extend(palette.iter().map(|bgr| RGBA { r: bgr.r, g: bgr.g, b: bgr.b, a: 0xFF }));
            rgba_palette.resize(max_palette_size, RGBA { r: 0, g: 0, b: 0, a: 0xFF });
        }
    }

    Ok(rgba_palette)
}


/*
Constructing output image from color index, genericized to minimize matching within hot loop
 */

enum ScanlineOrder {
    TopDown,
    BottomUp
}


fn write_image<E: EncodingMethod> (buf: &[u8], encoding_method: &E, order: ScanlineOrder, width: u64, height: u64) -> Result<RGBAImage, Error> {
    let stride: usize = encoding_method.stride(width).try_into()?;
    let mut remaining_bitmap_data = buf;

    let mut image_data: Vec<RGBA> = Vec::new();
    let image_data_size: usize = (width*height).try_into()?;
    image_data.reserve_exact(image_data_size);
    image_data.resize(image_data_size, RGBA { r: 0, g: 0, b: 0, a: 0 });
    let mut remaining_image_data: &mut [RGBA] = &mut image_data;

    let width_split: usize = width.try_into()?;

    match order {
        ScanlineOrder::TopDown => {
            for _ in 0..height {
                let curr_bitmap_scanline;
                (curr_bitmap_scanline, remaining_bitmap_data) = remaining_bitmap_data.split_at(stride);
                let curr_image_buf;
                (curr_image_buf, remaining_image_data) = remaining_image_data.split_at_mut(width_split);
                encoding_method.decode_scanline(curr_bitmap_scanline, curr_image_buf);
            }
        }
        ScanlineOrder::BottomUp => {
            for _ in 0..height {
                let curr_bitmap_scanline;
                (curr_bitmap_scanline, remaining_bitmap_data) = remaining_bitmap_data.split_at(stride);
                let curr_image_buf;
                (remaining_image_data, curr_image_buf) = remaining_image_data.split_at_mut(remaining_image_data.len()-width_split);
                encoding_method.decode_scanline(curr_bitmap_scanline, curr_image_buf);
            }
        }
    }

    Ok(RGBAImage { width, height, data: image_data.into_boxed_slice() })
}


/*
Main decode entry point
 */
pub fn decode(rest: &[u8]) -> Result<RGBAImage, Error> {
    // parse header, verify magic number is correct
    let (file_header_bytes, rest) = rest.split_at(BITMAP_FILE_HEADER_SIZE.try_into()?);
    let file_header: &BitmapFileHeader = from_bytes(file_header_bytes);
    if file_header.magic_number != BITMAP_MAGIC_NUMBER {
        bail!("Incorrect magic number of {:?}", file_header.magic_number)
    }
    let index_bytes = &rest[usize::try_from(file_header.bitmap_offset-BITMAP_FILE_HEADER_SIZE)?..];


    // info header fields must be read differently depending on the version of the info header
    // version is deduced by the headers' size (stored as the first 4 bytes as DWORD) //(i32, i32, BitDepth, Compression, &[u8], PaletteFormat)
    let info_header_size = u32::from_le_bytes(rest[0..4].try_into()?);
    let palette_size = NonZeroU32::new(file_header.bitmap_offset-info_header_size-BITMAP_FILE_HEADER_SIZE);
    let (width, height_temp, bit_depth, compression, rest, palette_format, bitmask_red, bitmask_green, bitmask_blue) = match info_header_size {

        // header is BITMAPCOREHEADER
        BITMAP_CORE_HEADER_SIZE => {
            let (header_bytes, rest) = rest.split_at(BITMAP_CORE_HEADER_SIZE.try_into()?);
            let h: &BitmapCoreHeader = from_bytes(header_bytes);

            // It is ok for us to return junk data as bitmask values because there is no bitfield compression
            // for this header type anyways. Furthermore, passing all ones would trigger an obvious
            // error in the constructor for bitfield encoding types.
            Ok((h.width.into(), h.height.into(), BitDepth::try_from(h.bit_depth)?, Compression::RGB, rest, PaletteFormat::BGRTriple, 0xFFFFu32, 0xFFFFu32, 0xFFFFu32))
        },

        // header is BITMAPINFOHEADER (the vast majority of images are of this type)
        BITMAP_INFO_HEADER_SIZE => {
            let header_bytes;
            let mut new_rest: &[u8];
            (header_bytes, new_rest) = rest.split_at(BITMAP_INFO_HEADER_SIZE.try_into()?);
            let h: &BitmapInfoHeader = from_bytes(header_bytes);

            // For this header type only, if bitfield compression is specified, the bit masks are instead
            // stored in 3 DWORDs in the palette field (after the header and before the bitmap)
            let compression = Compression::try_from(h.compression)?;
            let red_mask;
            let green_mask;
            let blue_mask;
            if compression == Compression::Bitfields {
                let mut mask_bytes;
                (mask_bytes, new_rest) = new_rest.split_at(4);
                red_mask = u32::from_le_bytes(mask_bytes.try_into()?);
                (mask_bytes, new_rest) = new_rest.split_at(4);
                green_mask = u32::from_le_bytes(mask_bytes.try_into()?);
                (mask_bytes, new_rest) = new_rest.split_at(4);
                blue_mask = u32::from_le_bytes(mask_bytes.try_into()?);
            }
            else {
                red_mask = 0xFFFF;
                green_mask = 0xFFFF;
                blue_mask = 0xFFFF;
            }

            Ok((h.width, h.height, BitDepth::try_from(h.bit_depth)?, compression, new_rest, PaletteFormat::BGRQuad, red_mask, green_mask, blue_mask))
        },

        BITMAP_V4_HEADER_SIZE => {
            let (header_bytes, rest) = rest.split_at(BITMAP_V4_HEADER_SIZE.try_into()?);
            let h: &BitmapV4Header = from_bytes(header_bytes);

            Ok((h.width, h.height, BitDepth::try_from(h.bit_depth)?, Compression::try_from(h.compression)?, rest, PaletteFormat::BGRQuad, h.red_mask, h.green_mask, h.blue_mask))
        },

        BITMAP_V5_HEADER_SIZE => {
            let (header_bytes, rest) = rest.split_at(BITMAP_V5_HEADER_SIZE.try_into()?);
            let h: &BitmapV5Header = from_bytes(header_bytes);

            Ok((h.width, h.height, BitDepth::try_from(h.bit_depth)?, Compression::try_from(h.compression)?, rest, PaletteFormat::BGRQuad, h.red_mask, h.green_mask, h.blue_mask))
        },

        _ => {
            Err(anyhow!("Unrecognized bitmap info header (size was {info_header_size})"))
        }
    }?;

    // double check that palette size makes sense for the given bit depth
    println!("Bit depth: {}", u16::from(bit_depth));
    let rest = match bit_depth {
        BitDepth::One | BitDepth::Two | BitDepth::Four | BitDepth::Eight => {
            if palette_size.is_none() {
                bail!("Paletted bit depth ({}) is missing a palette", u16::from(bit_depth))
            }
            rest
        },
        // This arm guards against a palette warning showing up for bitfield compression types, as
        // some headers use the palette area to store the bitmasks. This arm simply suppresses the
        // warning and correction that would otherwise be incorrectly applied in these circumstances.
        BitDepth::Sixteen | BitDepth::ThirtyTwo if compression == Compression::Bitfields => {
            rest
        },
        _ => {
            if let Some(nz_size) = palette_size {
                println!("WARNING: Bit depth of {} has non-zero palette size of {}", u16::from(bit_depth), nz_size);
                let (_, rest) = rest.split_at(nz_size.get().try_into()?); // ensure that palette is skipped for non-paletted images with extraneous palettes
                rest
            }
            else {
                rest
            }
        }
    };

    // width should be non-zero
    match width.cmp(&0) {
        Ordering::Less => {
            bail!("Width is a negative number ({width})")
        }
        Ordering::Equal => {
            println!("WARNING: Width is zero");
        }
        _ => {}
    }
    let width_converted: u64 = width.try_into()?;

    // height
    let scanline_order = if height_temp < 0 { ScanlineOrder::TopDown } else { ScanlineOrder::BottomUp };
    let height: u64 = height_temp.abs().try_into()?;
    if height_temp == 0 {
        println!("WARNING: Height is zero");
    }

    // sort into differing scanline decode methods based on bit depth and compression type
    match bit_depth {
        BitDepth::One => {
            if compression != Compression::RGB {
                bail!("Bit depth of 1 only supports RGB compression (used {compression:#?})")
            }
            let rgba_palette = create_palette(rest, bit_depth.into(), palette_size.unwrap(), &palette_format)?;
            let decode_method = Paletted1 { palette: &rgba_palette };
            write_image(index_bytes, &decode_method, scanline_order, width_converted, height)
        }
        BitDepth::Two => {
            if compression != Compression::RGB {
                bail!("Bit depth of 2 only supports RGB compression (used {compression:#?})")
            }
            let rgba_palette = create_palette(rest, bit_depth.into(), palette_size.unwrap(), &palette_format)?;
            let decode_method = Paletted2 { palette: &rgba_palette };
            write_image(index_bytes, &decode_method, scanline_order, width_converted, height)
        }
        BitDepth::Four => {
            if compression != Compression::RGB {
                bail!("Bit depth of 4 only supports RGB compression (used {compression:#?})")
            }
            let rgba_palette = create_palette(rest, bit_depth.into(), palette_size.unwrap(), &palette_format)?;
            let decode_method = Paletted4 { palette: &rgba_palette };
            write_image(index_bytes, &decode_method, scanline_order, width_converted, height)
        }
        BitDepth::Eight => {
            if compression != Compression::RGB {
                bail!("Bit depth of 8 only supports RGB compression (used {compression:#?})")
            }
            let rgba_palette = create_palette(rest, bit_depth.into(), palette_size.unwrap(), &palette_format)?;
            let decode_method = Paletted8 { palette: &rgba_palette };
            write_image(index_bytes, &decode_method, scanline_order, width_converted, height)
        }
        BitDepth::Sixteen => {
            match compression {
                Compression::RGB => {
                    let encoding_method = Uncompressed16 {};
                    write_image(index_bytes, &encoding_method, scanline_order, width_converted, height)
                }
                Compression::Bitfields => {
                    let encoding_method = Bitfield16::new(bitmask_red, bitmask_green, bitmask_blue)?;
                    write_image(index_bytes, &encoding_method, scanline_order, width_converted, height)
                }
                _ => bail!("Bit depth of 16 only supports RGB compression or bitfield compression (used {compression:#?})")
            }
        }
        BitDepth::TwentyFour => {
            if compression != Compression::RGB {
                bail!("Bit depth of 24 only supports RGB compression (used {compression:#?})")
            }
            let decode_method = Uncompressed24 {};
            write_image(index_bytes, &decode_method, scanline_order, width_converted, height)
        }
        BitDepth::ThirtyTwo => {
            match compression {
                Compression::RGB => {
                    let encoding_method = Uncompressed32 {};
                    write_image(index_bytes, &encoding_method, scanline_order, width_converted, height)
                }
                Compression::Bitfields => {
                    let encoding_method = Bitfield32::new(bitmask_red, bitmask_green, bitmask_blue)?;
                    write_image(index_bytes, &encoding_method, scanline_order, width_converted, height)
                }
                _ => bail!("Bit depth of 32 only supports RGB compression or bitfield compression (used {compression:#?})")
            }
        }
    } 
}


pub fn encode(image: &RGBAImage) -> Result<Box<[u8]>, Box<dyn std::error::Error>> {
    // calculate scanline width in bytes
    let scanline_width: usize = (Uncompressed24 {}).stride(image.width).try_into()?;

    // buffer size should be file header + info header + 3 bytes per pixel color - no palette needed
    let buffer_size: usize = (u64::from(BITMAP_FILE_HEADER_SIZE) + u64::from(BITMAP_INFO_HEADER_SIZE) + u64::try_from(scanline_width)?*image.height).try_into()?;
    println!("Encoded size is {buffer_size} bytes");
    let mut buf: Vec<u8> = Vec::new();
    buf.reserve_exact(buffer_size);
    buf.resize(buffer_size, 0);

    // set file header
    let (file_header_bytes, rest) = buf.split_at_mut(BITMAP_FILE_HEADER_SIZE.try_into()?);
    let file_header: &mut BitmapFileHeader = from_bytes_mut(file_header_bytes);
    file_header.magic_number = BITMAP_MAGIC_NUMBER;
    file_header.file_size = buffer_size.try_into()?;
    file_header.bitmap_offset = BITMAP_FILE_HEADER_SIZE + BITMAP_INFO_HEADER_SIZE;

    // set info header - we use BitmapInfoHeader as it is the most widely supported and most commonly used
    let (info_header_bytes, color_bytes) = rest.split_at_mut(BITMAP_INFO_HEADER_SIZE.try_into()?);
    let info_header: &mut BitmapInfoHeader = from_bytes_mut(info_header_bytes);
    info_header.header_size = BITMAP_INFO_HEADER_SIZE;
    info_header.width = image.width.try_into()?;
    info_header.height = image.height.try_into()?;
    info_header.planes = 1;
    info_header.bit_depth = 24;
    info_header.compression = Compression::RGB.into();
    info_header.image_size = 0;
    info_header.x_pixels_per_meter = 2835;
    info_header.y_pixels_per_meter = 2835;
    info_header.colors_used = 0; // no palette
    info_header.important_colors_used = 0;

    // write true color data in scanlines from bottom up (as mandated by a positive height)
    let mut source_offset = 0;
    for scanline in (0..usize::try_from(image.height)?).rev() {
        let mut bitmap_offset = scanline*scanline_width;
        for _ in 0..image.width {
            color_bytes[bitmap_offset] = image.data[source_offset].b;
            color_bytes[bitmap_offset+1] = image.data[source_offset].g;
            color_bytes[bitmap_offset+2] = image.data[source_offset].r;
            source_offset += 1;
            bitmap_offset += 3;
        }
    }

    Ok(buf.into_boxed_slice())
}


/*
Unit tests
 */
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_struct_sizes_and_alignment() {
        assert_eq!(std::mem::size_of::<BitmapFileHeader>(), 14);
        assert_eq!(std::mem::size_of::<BitmapFileHeader>(), BITMAP_FILE_HEADER_SIZE.try_into().unwrap());
        assert_eq!(std::mem::align_of::<BitmapFileHeader>(), 1);

        assert_eq!(std::mem::size_of::<BitmapCoreHeader>(), 12);
        assert_eq!(std::mem::size_of::<BitmapCoreHeader>(), BITMAP_CORE_HEADER_SIZE.try_into().unwrap());
        assert_eq!(std::mem::align_of::<BitmapCoreHeader>(), 1);

        assert_eq!(std::mem::size_of::<BitmapInfoHeader>(), 40);
        assert_eq!(std::mem::size_of::<BitmapInfoHeader>(), BITMAP_INFO_HEADER_SIZE.try_into().unwrap());
        assert_eq!(std::mem::align_of::<BitmapInfoHeader>(), 1);

        assert_eq!(std::mem::size_of::<BitmapV4Header>(), 108);
        assert_eq!(std::mem::size_of::<BitmapV4Header>(), BITMAP_V4_HEADER_SIZE.try_into().unwrap());
        assert_eq!(std::mem::align_of::<BitmapV4Header>(), 1);

        assert_eq!(std::mem::size_of::<BitmapV5Header>(), 124);
        assert_eq!(std::mem::size_of::<BitmapV5Header>(), BITMAP_V5_HEADER_SIZE.try_into().unwrap());
        assert_eq!(std::mem::align_of::<BitmapV5Header>(), 1);

        assert_eq!(std::mem::size_of::<BGRQuad>(), 4);
        assert_eq!(std::mem::size_of::<BGRTriple>(), 3);
    }
}