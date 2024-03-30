use std::cmp::Ordering;
use std::convert::TryInto;
use std::fmt::{Debug, Display, Formatter};
use std::num::NonZeroU32;
use bytemuck::{cast_slice, from_bytes, from_bytes_mut, Pod, Zeroable};
use num_enum::{IntoPrimitive, TryFromPrimitive};
use crate::common::{RGBA, RGBAImage};


/*
Errors
 */

#[derive(Debug)]
struct ParseError {
    details: String
}

impl ParseError {
    fn new(details: String) -> Box<ParseError> {
        Box::new(ParseError {details})
    }
}

impl std::error::Error for ParseError {}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "ParseError: {}", self.details)
    }
}


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
Compression types
 */

#[derive(Debug, IntoPrimitive, TryFromPrimitive)]
#[repr(u32)]
enum Compression {
    RGB = 0x0,
    RLE8 = 0x1,
    RLE4 = 0x2,
    Bitfields = 0x3,
    JPEG = 0x4,
    PNG = 0x5,
    CMYK = 0xB,
    CMYKRLE8 = 0xC,
    CMYKRLE4 = 0xD
}

// constants documented in https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-wmf/4e588f70-bd92-4a6f-b77f-35d0feaf7a57
impl Compression {
    fn new(val: u32) -> Result<Compression, Box<dyn std::error::Error>> {
        use Compression::*;
        let compression = Compression::try_from(val)?;
        match compression {
            RGB | RLE8 | RLE4 | Bitfields => {
                Ok(compression)
            }
            _ => {
                Err(ParseError::new(format!("Unsupported compression type ({compression:#?})")))
            }
        }
    }
}


/*
Bit depths
 */

struct DepthOne;
struct DepthTwo;
struct DepthFour;
struct DepthEight;
struct DepthSixteen;
struct DepthTwentyFour;
struct DepthThirtyTwo;

trait Depth {
    fn get(&self) -> u64;
    fn stride(&self, width: u64) -> u64;
}

pub enum BitDepth {
    One(DepthOne),
    Two(DepthTwo),
    Four(DepthFour),
    Eight(DepthEight),
    Sixteen(DepthSixteen),
    TwentyFour(DepthTwentyFour),
    ThirtyTwo(DepthThirtyTwo)
}

impl BitDepth {
    fn new(depth: u16) -> Result<BitDepth, Box<ParseError>> {
        match depth {
            1 => Ok(BitDepth::One(DepthOne {})),
            2 => Ok(BitDepth::Two(DepthTwo {})),
            4 => Ok(BitDepth::Four(DepthFour {})),
            8 => Ok(BitDepth::Eight(DepthEight {})),
            16 => Ok(BitDepth::Sixteen(DepthSixteen {})),
            24 => Ok(BitDepth::TwentyFour(DepthTwentyFour {})),
            32 => Ok(BitDepth::ThirtyTwo(DepthThirtyTwo {})),
            _ => Err(ParseError::new(format!("Invalid bit depth of {depth}")))
        }
    }

    fn get(&self) -> i32 {
        match self {
            BitDepth::One(_) => 1,
            BitDepth::Two(_) => 2,
            BitDepth::Four(_) => 4,
            BitDepth::Eight(_) => 8,
            BitDepth::Sixteen(_) => 16,
            BitDepth::TwentyFour(_) => 24,
            BitDepth::ThirtyTwo(_) => 32
        }
    }
}

impl Display for BitDepth {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.get())
    }
}


/*
Implementing paletted bit depths
 */

trait Paletted: Depth {
     fn write_paletted_bytes(&self, index_bytes: &[u8], image_buffer: &mut [RGBA], palette: &[RGBA]);
}

// depth one (monochrome) implementations
impl Depth for DepthOne {
    fn get(&self) -> u64 {
        1
    }

    fn stride(&self, width: u64) -> u64 {
        let data_bytes = width.div_ceil(8);
        data_bytes.div_ceil(4)*4
    }
}

impl Paletted for DepthOne {
    fn write_paletted_bytes(&self, index_bytes: &[u8], image_buffer: &mut [RGBA], palette: &[RGBA]) {
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
                *color = palette[0];
            }
            else {
                *color = palette[1];
            }
        }
    }
}

// depth two implementations
impl Depth for DepthTwo {
    fn get(&self) -> u64 {
        2
    }

    fn stride(&self, width: u64) -> u64 {
        let data_bytes = (width*2).div_ceil(8);
        data_bytes.div_ceil(4)*4
    }
}

impl Paletted for DepthTwo {
    fn write_paletted_bytes(&self, index_bytes: &[u8], image_buffer: &mut [RGBA], palette: &[RGBA]) {
        // suppose a 0x0 image is read, this ensures we exit normally (not writing anything into these 0-sized buffers
        // rather than panic
        if index_bytes.is_empty() {
            return;
        }

        let mut curr_byte = index_bytes[0]; // immediately overwritten in first iteration
        for (i, color) in image_buffer.iter_mut().enumerate() {
            // from most significant to least significant in a byte,
            // offset should go 6, 4, 2, 0
            let byte_offset = 8 - (i % 4) * 2;
            if byte_offset == 6 {
                let byte_num = i / 4;
                curr_byte = index_bytes[byte_num];
            }

            let index = (curr_byte >> byte_offset) & 0b11;
            *color = palette[usize::from(index)];
        }
    }
}

// depth four implementations
impl Depth for DepthFour {
    fn get(&self) -> u64 {
        4
    }

    fn stride(&self, width: u64) -> u64 {
        let data_bytes = (width*4).div_ceil(8);
        data_bytes.div_ceil(4)*4
    }
}

impl Paletted for DepthFour {
    fn write_paletted_bytes(&self, index_bytes: &[u8], image_buffer: &mut [RGBA], palette: &[RGBA]) {
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
            *color = palette[usize::from(index)];
        }
    }
}

// depth eight implementations
impl Depth for DepthEight {
    fn get(&self) -> u64 {
        8
    }

    fn stride(&self, width: u64) -> u64 {
        width.div_ceil(4)*4
    }
}

impl Paletted for DepthEight {
    fn write_paletted_bytes(&self, index_bytes: &[u8], image_buffer: &mut [RGBA], palette: &[RGBA]) {
        for (i, color) in image_buffer.iter_mut().enumerate() {
            *color = palette[usize::from(index_bytes[i])];
        }
    }
}


/*
Implementing unpaletted bit depths
 */

trait Unpaletted: Depth {
    fn write_unpaletted_bytes(&self, index_bytes: &[u8], image_buffer: &mut [RGBA]);
}

// depth sixteen implementations
impl Depth for DepthSixteen {
    fn get(&self) -> u64 {
        16
    }

    fn stride(&self, width: u64) -> u64 {
        let data_bytes = width*2;
        data_bytes.div_ceil(4)*4
    }
}


impl Unpaletted for DepthSixteen {
    #[allow(clippy::cast_possible_truncation)]
    fn write_unpaletted_bytes(&self, index_bytes: &[u8], image_buffer: &mut [RGBA]) {
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

// depth twenty-four implementations
impl Depth for DepthTwentyFour {
    fn get(&self) -> u64 {
        24
    }

    fn stride(&self, width: u64) -> u64 {
        let data_bytes = width*3;
        data_bytes.div_ceil(4)*4
    }
}

impl Unpaletted for DepthTwentyFour {
    fn write_unpaletted_bytes(&self, index_bytes: &[u8], image_buffer: &mut [RGBA]) {
        let mut index_offset = 0;
        for color in image_buffer {
            color.b = index_bytes[index_offset];
            color.g = index_bytes[index_offset+1];
            color.r = index_bytes[index_offset+2];
            index_offset += 3;
        }
    }
}

// depth thirty-two implementations
impl Depth for DepthThirtyTwo {
    fn get(&self) -> u64 {
        32
    }

    fn stride(&self, width: u64) -> u64 {
        width*4
    }
}

impl Unpaletted for DepthThirtyTwo {
    fn write_unpaletted_bytes(&self, index_bytes: &[u8], image_buffer: &mut [RGBA]) {
        let mut index_offset = 0;
        for color in image_buffer {
            // high byte unused
            color.b = index_bytes[index_offset];
            color.g = index_bytes[index_offset+1];
            color.r = index_bytes[index_offset+2];
            index_offset += 4;
        }
    }
}


/*
Implementing bitfield-using bitdepths
 */

trait BitField {
    fn write_bitfield_bytes(&self, index_bytes: &[u8], image_buffer: &mut [RGBA]);
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

fn create_palette<'a, D: Paletted>(buf: &'a[u8], bit_depth: &D, palette_size: NonZeroU32, palette_format: PaletteFormat) -> Result<(Vec<RGBA>, &'a[u8]), Box<dyn std::error::Error>> {
    // rgba_palette is extended to include all possible index values for the given bit depth (2^bit_depth)
    // ensuring that invalid indexes result in a black pixel by default rather than a panic
    let max_palette_size: usize = 1 << bit_depth.get();
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

    Ok((rgba_palette, index_bytes))
}


/*
Constructing output image from color index, genericized to minimize matching within hot loop
 */

enum ScanlineOrder {
    TopDown,
    BottomUp
}


fn write_paletted_image<D: Paletted> (buf: &[u8], rgba_palette: &[RGBA], depth: &D, order: ScanlineOrder, width: u64, height: u64) -> Result<RGBAImage, Box<dyn std::error::Error>> {
    let stride: usize = depth.stride(width).try_into()?;
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
                depth.write_paletted_bytes(curr_bitmap_scanline, curr_image_buf, &rgba_palette);
            }
        }
        ScanlineOrder::BottomUp => {
            for _ in 0..height {
                let curr_bitmap_scanline;
                (curr_bitmap_scanline, remaining_bitmap_data) = remaining_bitmap_data.split_at(stride);
                let curr_image_buf;
                (remaining_image_data, curr_image_buf) = remaining_image_data.split_at_mut(remaining_image_data.len()-width_split);
                depth.write_paletted_bytes(curr_bitmap_scanline, curr_image_buf, &rgba_palette);
            }
        }
    }

    Ok(RGBAImage { width, height, data: image_data.into_boxed_slice() })
}


fn write_unpaletted_image<D: Unpaletted>(buf: &[u8], depth: &D, order: ScanlineOrder, width: u64, height: u64) -> Result<RGBAImage, Box<dyn std::error::Error>> {
    let stride: usize = depth.stride(width).try_into()?;

    let mut image_data: Vec<RGBA> = Vec::new();
    let image_data_size: usize = (width*height).try_into()?;
    image_data.reserve_exact(image_data_size);
    image_data.resize(image_data_size, RGBA { r: 0, g: 0, b: 0, a: 0 });
    let mut remaining_image_data: &mut [RGBA] = &mut image_data;

    let width_split: usize = width.try_into()?;
    let mut remaining_bitmap_data = buf;

    match order {
        ScanlineOrder::TopDown => {
            for _ in 0..height {
                let curr_bitmap_scanline;
                (curr_bitmap_scanline, remaining_bitmap_data) = remaining_bitmap_data.split_at(stride);
                let curr_image_buf;
                (curr_image_buf, remaining_image_data) = remaining_image_data.split_at_mut(width_split);
                depth.write_unpaletted_bytes(curr_bitmap_scanline, curr_image_buf);
            }
        }
        ScanlineOrder::BottomUp => {
            for _ in 0..height {
                let curr_bitmap_scanline;
                (curr_bitmap_scanline, remaining_bitmap_data) = remaining_bitmap_data.split_at(stride);
                let curr_image_buf;
                (remaining_image_data, curr_image_buf) = remaining_image_data.split_at_mut(remaining_image_data.len()-width_split);
                depth.write_unpaletted_bytes(curr_bitmap_scanline, curr_image_buf);
            }
        }
    }

    Ok(RGBAImage { width, height, data: image_data.into_boxed_slice() })
}


/*
Main decode entry point
 */
pub fn decode(rest: &[u8]) -> Result<RGBAImage, Box<dyn std::error::Error>> {

    // parse header, verify magic number is correct
    let (file_header_bytes, rest) = rest.split_at(BITMAP_FILE_HEADER_SIZE.try_into()?);
    let file_header: &BitmapFileHeader = from_bytes(file_header_bytes);
    if file_header.magic_number != BITMAP_MAGIC_NUMBER {
        return Err(ParseError::new(format!("Incorrect magic number of {:?}", file_header.magic_number)));
    }


    // info header fields must be read differently depending on the version of the info header
    // version is deduced by the headers' size (stored as the first 4 bytes as DWORD)
    let info_header_size = u32::from_le_bytes(rest[0..4].try_into()?);
    let t: u32 = file_header.bitmap_offset;
    let palette_size = NonZeroU32::new(file_header.bitmap_offset-info_header_size-BITMAP_FILE_HEADER_SIZE);
    let (width, height_temp, bit_depth, compression, rest, palette_format): (i32, i32, BitDepth, Compression, &[u8], PaletteFormat) = match info_header_size {

        // header is BITMAPCOREHEADER
        BITMAP_CORE_HEADER_SIZE => {
            let (header_bytes, rest) = rest.split_at(BITMAP_CORE_HEADER_SIZE.try_into()?);
            let h: &BitmapCoreHeader = from_bytes(header_bytes);

            Ok((h.width.into(), h.height.into(), BitDepth::new(h.bit_depth)?, Compression::RGB, rest, PaletteFormat::BGRTriple))
        },

        // header is BITMAPINFOHEADER (the vast majority of images are of this type)
        BITMAP_INFO_HEADER_SIZE => {
            let (header_bytes, rest) = rest.split_at(BITMAP_INFO_HEADER_SIZE.try_into()?);
            let h: &BitmapInfoHeader = from_bytes(header_bytes);

            Ok((h.width, h.height, BitDepth::new(h.bit_depth)?, Compression::new(h.compression)?, rest, PaletteFormat::BGRQuad))
        },

        BITMAP_V4_HEADER_SIZE => {
            let (header_bytes, rest) = rest.split_at(BITMAP_V4_HEADER_SIZE.try_into()?);
            let h: &BitmapV4Header = from_bytes(header_bytes);

            Ok((h.width, h.height, BitDepth::new(h.bit_depth)?, Compression::new(h.compression)?, rest, PaletteFormat::BGRQuad))
        },

        BITMAP_V5_HEADER_SIZE => {
            let (header_bytes, rest) = rest.split_at(BITMAP_V5_HEADER_SIZE.try_into()?);
            let h: &BitmapV5Header = from_bytes(header_bytes);

            Ok((h.width, h.height, BitDepth::new(h.bit_depth)?, Compression::new(h.compression)?, rest, PaletteFormat::BGRQuad))
        },

        _ => {
            Err(ParseError::new(format!("Unrecognized bitmap info header (size was {info_header_size})")))
        }
    }?;

    // double check that palette size makes sense for the given bit depth
    println!("Bit depth: {}", bit_depth);
    let rest = match bit_depth {
        BitDepth::One(_) | BitDepth::Two(_) | BitDepth::Four(_) | BitDepth::Eight(_) => {
            if palette_size.is_none() {
                return Err(ParseError::new(format!("Paletted bit depth ({bit_depth}) is missing a palette")));
            }
            rest
        }
        _ => {
            if let Some(nz_size) = palette_size {
                println!("WARNING: Bit depth of {bit_depth} has non-zero palette size of {nz_size}");
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
            return Err(ParseError::new(format!("Width is a negative number ({width})")));
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

    // sort into generic paletted or unpaletted functions to write the actual output
    match bit_depth {
        BitDepth::One(d) => {
            let (rgba_palette, rest) = create_palette(rest, &d, palette_size.unwrap(), palette_format)?;
            write_paletted_image(rest, &rgba_palette, &d, scanline_order, width_converted, height)
        },
        BitDepth::Two(d) => {
            let (rgba_palette, rest) = create_palette(rest, &d, palette_size.unwrap(), palette_format)?;
            write_paletted_image(rest, &rgba_palette, &d, scanline_order, width_converted, height)
        },
        BitDepth::Four(d) => {
            let (rgba_palette, rest) = create_palette(rest, &d, palette_size.unwrap(), palette_format)?;
            write_paletted_image(rest, &rgba_palette, &d, scanline_order, width_converted, height)
        },
        BitDepth::Eight(d) => {
            let (rgba_palette, rest) = create_palette(rest, &d, palette_size.unwrap(), palette_format)?;
            write_paletted_image(rest, &rgba_palette, &d, scanline_order, width_converted, height)
        },
        BitDepth::Sixteen(d) => {
            write_unpaletted_image(rest, &d, scanline_order, width_converted, height)
        }
        BitDepth::TwentyFour(d) => {
            write_unpaletted_image(rest, &d, scanline_order, width_converted, height)
        }
        BitDepth::ThirtyTwo(d) => {
            match compression {
                Compression::RGB => {
                    write_unpaletted_image(rest, &d, scanline_order, width_converted, height)
                },
                _ => Err(ParseError::new(format!("Bit depth of 32 has invalid compression type of {compression:#?}")))
            }
        }
    }
}


pub fn encode(image: &RGBAImage) -> Result<Box<[u8]>, Box<dyn std::error::Error>> {
    // calculate scanline width in bytes
    let scanline_width: usize = DepthTwentyFour.stride(image.width).try_into()?;

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