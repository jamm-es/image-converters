use bytemuck::{Pod, Zeroable};

#[derive(Pod, Zeroable, Copy, Clone)]
#[repr(C)]
pub struct RGBA {
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub a: u8
}


pub struct RGBAImage {
    pub width: u64,
    pub height: u64,
    pub data: Box<[RGBA]>
}