use std::convert::TryInto;
use std::fmt::{Debug, Display, Formatter};
use std::num::NonZeroU32;
use bytemuck::{cast_slice, from_bytes, from_bytes_mut, Pod, Zeroable};
use crate::common::{RGBA, RGBAImage};


/*
Errors
 */

enum ParseError {
    WrongMagicNumber,
    UnrecognizedBitmapInfoHeader,
    InvalidBitDepth,
    MissingRequiredPalette,
    UnsupportedCompressionType,
    InvalidWidth
}

impl std::error::Error for ParseError {}

impl Debug for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let msg = match self {
            ParseError::WrongMagicNumber => "Wrong magic number",
            ParseError::UnrecognizedBitmapInfoHeader => "Unrecognized bitmap info header",
            ParseError::InvalidBitDepth => "Invalid bit depth",
            ParseError::MissingRequiredPalette => "Missing required palette",
            ParseError::UnsupportedCompressionType => "Unsupported compression type",
            ParseError::InvalidWidth => "Invalid width"
        };

        write!(f, "ParseError: {msg}")
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
const RGB_COMPRESSION_TYPE: u32 = 0;
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
    fn stride(&self, width: u64) -> u64 {
        width*u64::from(self.get()).div_ceil(8)
    }
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
            _ => Err(Box::new(ParseError::InvalidBitDepth))
        }
    }
}

impl Display for BitDepth {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let num = match self {
            BitDepth::One(_) => 1,
            BitDepth::Two(_) => 2,
            BitDepth::Four(_) => 4,
            BitDepth::Eight(_) => 8,
            BitDepth::Sixteen(_) => 16,
            BitDepth::TwentyFour(_) => 24,
            BitDepth::ThirtyTwo(_) => 32
        };

        write!(f, "{num}")
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
        width*self.get().div_ceil(8)
    }
}

impl Paletted for DepthOne {
    fn write_paletted_bytes(&self, index_bytes: &[u8], image_buffer: &mut [RGBA], palette: &[RGBA]) {
        // suppose a 0x0 image is read, this ensures we exit normally (not writing anything into these 0-sized buffers
        // rather than panic
        if index_bytes.len() == 0 {
            return;
        }

        let mut i = 0;
        let mut curr_byte = index_bytes[0]; // immediately overwritten in first iter
        for color in image_buffer {
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

            i += 1;
        }
    }
}

// depth two implementations
impl Depth for DepthTwo {
    fn get(&self) -> u64 {
        2
    }

    fn stride(&self, width: u64) -> u64 {
        width*self.get().div_ceil(8)
    }
}

impl Paletted for DepthTwo {
    fn write_paletted_bytes(&self, index_bytes: &[u8], image_buffer: &mut [RGBA], palette: &[RGBA]) {
        // suppose a 0x0 image is read, this ensures we exit normally (not writing anything into these 0-sized buffers
        // rather than panic
        if index_bytes.len() == 0 {
            return;
        }

        let mut i = 0;
        let mut curr_byte = index_bytes[0]; // immediately overwritten in first iteration
        for color in image_buffer {
            // from most significant to least significant in a byte,
            // offset should go 6, 4, 2, 0
            let byte_offset = 8 - (i % 4) * 2;
            if byte_offset == 6 {
                let byte_num = i / 4;
                curr_byte = index_bytes[byte_num];
            }

            let index = (curr_byte >> byte_offset) & 0b11;
            *color = palette[usize::from(index)];

            i += 1;
        }
    }
}

// depth four implementations
impl Depth for DepthFour {
    fn get(&self) -> u64 {
        4
    }

    fn stride(&self, width: u64) -> u64 {
        width*self.get().div_ceil(8)
    }
}

impl Paletted for DepthFour {
    fn write_paletted_bytes(&self, index_bytes: &[u8], image_buffer: &mut [RGBA], palette: &[RGBA]) {
        // suppose a 0x0 image is read, this ensures we exit normally (not writing anything into these 0-sized buffers
        // rather than panic
        if index_bytes.len() == 0 {
            return;
        }

        let mut i = 0;
        let mut curr_byte = index_bytes[0]; // immediately overwritten in first iteration
        for color in image_buffer {
            // from most significant to least significant in a byte,
            // offset should go 6, 4, 2, 0
            let is_upper_half = i % 2 == 0;
            if is_upper_half {
                let byte_num = i / 2;
                curr_byte = index_bytes[byte_num];
            }

            let index = if is_upper_half { curr_byte >> 4 } else { curr_byte & 0b1111 };
            *color = palette[usize::from(index)];

            i += 1;
        }
    }
}

// depth eight implementations
impl Depth for DepthEight {
    fn get(&self) -> u64 {
        8
    }

    fn stride(&self, width: u64) -> u64 {
        width
    }
}

impl Paletted for DepthEight {
    fn write_paletted_bytes(&self, index_bytes: &[u8], image_buffer: &mut [RGBA], palette: &[RGBA]) {
        let mut i = 0;
        for color in image_buffer {
            *color = palette[usize::from(index_bytes[i])];
            i += 1;
        }
    }
}


/*
Implementing unpaletted bit depths
 */

trait Unpaletted: Depth {
    fn write_unpaletted_bytes(&self, index_bytes: &[u8], image_buffer: &mut [RGBA]);
}

// depth eight implementations
impl Depth for DepthSixteen {
    fn get(&self) -> u64 {
        8
    }

    fn stride(&self, width: u64) -> u64 {
        width
    }
}

impl Unpaletted for DepthSixteen {
    fn write_unpaletted_bytes(&self, index_bytes: &[u8], image_buffer: &mut [RGBA]) {
    }
}


/*
Palette parsing
 */

#[derive(Pod, Zeroable, Copy, Clone)]
#[repr(C)]
struct BGRQuad {
    b: u8,
    g: u8,
    r: u8,
    reserved: u8
}

fn create_palette<'a, D: Paletted>(buf: &'a[u8], bit_depth: &D, palette_size: NonZeroU32) -> Result<(Vec<RGBA>, &'a[u8]), Box<dyn std::error::Error>> {
    // create palette for color index section
    let (palette_bytes, index_bytes) = buf.split_at(palette_size.get().try_into()?);
    let palette: &[BGRQuad] = cast_slice(palette_bytes);
    println!("Calculated palette size of {} ({} colors)", palette_size, palette_size.get()/4);

    // rgba_palette is extended to include all possible index values for the given bit depth (2^bit_depth)
    // ensuring that invalid indexes result in a black pixel by default rather than a panic
    let max_palette_size: usize = 1 << bit_depth.get();
    let mut rgba_palette: Vec<RGBA> = Vec::new();
    rgba_palette.reserve_exact(max_palette_size);
    rgba_palette.extend(palette.iter().map(|bgr| RGBA { r: bgr.r, g: bgr.g, b: bgr.b, a: 0xFF }));
    rgba_palette.resize(max_palette_size, RGBA { r: 0, g: 0, b: 0, a: 0xFF });

    Ok((rgba_palette, index_bytes))
}


/*
Constructing output image from color index, genericized to minimize matching within hot loop
 */

enum ScanlineOrder {
    TopDown,
    BottomUp
}


fn write_paletted_image<D: Paletted> (buf: &[u8], depth: &D, order: ScanlineOrder, width: u64, height: u64, palette_size: NonZeroU32) -> Result<RGBAImage, Box<dyn std::error::Error>> {
    let stride: usize = depth.stride(width).try_into()?;
    let rgba_palette;
    let mut remaining_bitmap_data;
    (rgba_palette, remaining_bitmap_data) = create_palette(buf, depth, palette_size)?;

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
        return Err(Box::new(ParseError::WrongMagicNumber));
    }


    // info header fields must be read differently depending on the version of the info header
    // version is deduced by the headers' size (stored as the first 4 bytes as DWORD)
    let info_header_size = u32::from_le_bytes(rest[0..4].try_into()?);
    let palette_size = NonZeroU32::new(file_header.bitmap_offset-info_header_size-BITMAP_FILE_HEADER_SIZE);
    let (width, height_temp, bit_depth, compression_type, rest): (i32, i32, BitDepth, u32, &[u8]) = match info_header_size {

        // header is BITMAPCOREHEADER
        BITMAP_CORE_HEADER_SIZE => {
            let (header_bytes, rest) = rest.split_at(BITMAP_CORE_HEADER_SIZE.try_into()?);
            let h: &BitmapCoreHeader = from_bytes(header_bytes);

            Ok((h.width.into(), h.height.into(), BitDepth::new(h.bit_depth)?, RGB_COMPRESSION_TYPE, rest))
        },

        // header is BITMAPINFOHEADER (the vast majority of images are of this type)
        BITMAP_INFO_HEADER_SIZE => {
            let (header_bytes, rest) = rest.split_at(BITMAP_INFO_HEADER_SIZE.try_into()?);
            let h: &BitmapInfoHeader = from_bytes(header_bytes);
            if let Some(p_size) = palette_size {
                if h.bit_depth <= 8 {
                    let actual_colors_used = if h.colors_used == 0 { 1 << h.bit_depth } else { h.colors_used };
                    if p_size.get() != 4*actual_colors_used {
                        println!("WARNING: Info header colors used field does not match with palette size calculated from offset");
                    }
                }
            }

            Ok((h.width, h.height, BitDepth::new(h.bit_depth)?, h.compression, rest))
        },

        _ => {
            Err(Box::new(ParseError::UnrecognizedBitmapInfoHeader))
        }
    }?;


    // we do not support non-RGB (uncompressed) images
    if compression_type != RGB_COMPRESSION_TYPE {
        return Err(Box::new(ParseError::UnsupportedCompressionType));
    }

    // double check that palette size makes sense for the given bit depth
    match bit_depth {
        BitDepth::One(_) | BitDepth::Two(_) | BitDepth::Four(_) | BitDepth::Eight(_) => {
            if palette_size.is_none() {
                return Err(Box::new(ParseError::MissingRequiredPalette));
            }
        }
        _ => {
            if let Some(nz_size) = palette_size {
                println!("WARNING: Bit depth of {bit_depth} has non-zero palette size of {nz_size}");
            }
        }
    };

    // width should be non-zero
    if width == 0 {
        println!("WARNING: Width is zero");
    }
    else if width < 0 {
        return Err(Box::new(ParseError::InvalidWidth));
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
            write_paletted_image(rest, &d, scanline_order, width_converted, height, palette_size.unwrap())
        },
        BitDepth::Two(d) => {
            write_paletted_image(rest, &d, scanline_order, width_converted, height, palette_size.unwrap())
        },
        BitDepth::Four(d) => {
            write_paletted_image(rest, &d, scanline_order, width_converted, height, palette_size.unwrap())
        },
        BitDepth::Eight(d) => {
            write_paletted_image(rest, &d, scanline_order, width_converted, height, palette_size.unwrap())
        },
        _ => {
            todo!()
        }
        // BitDepth::Sixteen(d) => {
        //     write_unpaletted_image(rest, &d, scanline_order, width_converted, height)
        // }
        // BitDepth::TwentyFour(d) => {
        //     write_unpaletted_image(rest, &d, scanline_order, width_converted, height)
        // }
        // BitDepth::ThirtyTwo(d) => {
        //     write_unpaletted_image(rest, &d, scanline_order, width_converted, height)
        // }
    }
}


fn encode(image: &RGBAImage) -> Result<Box<[u8]>, Box<dyn std::error::Error>> {
    // buffer size should be file header + info header + 3 bytes per pixel color - no palette needed
    let buffer_size: usize = (u64::from(BITMAP_FILE_HEADER_SIZE) + u64::from(BITMAP_INFO_HEADER_SIZE) + image.width*image.height*3).try_into()?;
    let mut buf: Vec<u8> = Vec::new();
    buf.reserve_exact(buffer_size);
    buf.resize(buffer_size, 0);

    // set file header
    let (file_header_bytes, rest) = buf.split_at_mut(BITMAP_FILE_HEADER_SIZE.try_into()?);
    let mut file_header: &BitmapFileHeader = from_bytes_mut(file_header_bytes);
    // file_header.magic


    Ok(buf.into_boxed_slice())
}