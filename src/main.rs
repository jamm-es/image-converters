use std::fmt::{Debug, Display};
use std::fs;
mod bmp;
mod common;


fn main() -> std::io::Result<()> {
    // let file_name = "rgb-3c-8b.bmp";
    let file_name = "bmpsuite-2.8/g/pal8os2.bmp";
    // let file_name = "bmpsuite-2.8/g/rgb24.bmp";
    println!("Reading file {}...", file_name);
    //
    //
    let raw_bmp = fs::read(file_name).unwrap();
    match bmp::decode(&raw_bmp) {
        Ok(img) => {
            println!("yay image ok!");
        }
        Err(err) => {
            println!("Yikes image error: {}", err.to_string());
        }
    }

    // let paths = fs::read_dir("samples").unwrap();
    // for path in paths {
    //     let p = path.unwrap().path();
    //     let p2 = p.to_str().unwrap();
    //     // println!("{}", p2);
    //     let raw_bmp = fs::read(p2).unwrap();
    //     match parse_bmp(&raw_bmp) {
    //         Ok(img) => {
    //             // println!("yay image ok!");
    //         }
    //         Err(err) => {
    //             println!("Yikes image error: {}", err.to_string());
    //         }
    //     }
    // }

    Ok(())
}

// #[cfg(test)]
// mod tests {
//     use crate::{BitmapCoreHeader, BitmapFileHeader, BitmapInfoHeader, BGRQuad};
//
//     #[test]
//     fn check_struct_sizes_and_alignment() {
//         assert_eq!(std::mem::size_of::<BitmapFileHeader>(), 14);
//         assert_eq!(std::mem::align_of::<BitmapFileHeader>(), 1);
//
//         assert_eq!(std::mem::size_of::<BitmapCoreHeader>(), 12);
//         assert_eq!(std::mem::align_of::<BitmapCoreHeader>(), 1);
//
//         assert_eq!(std::mem::size_of::<BitmapInfoHeader>(), 40);
//         assert_eq!(std::mem::align_of::<BitmapInfoHeader>(), 1);
//
//         assert_eq!(std::mem::size_of::<BGRQuad>(), 4);
//         assert_eq!(std::mem::align_of::<BitmapInfoHeader>(), 1);
//     }
// }
