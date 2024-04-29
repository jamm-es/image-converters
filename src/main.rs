use std::env;
use std::fs;
use std::process::ExitCode;

mod bmp;
mod common;


fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        println!("usage: imgcnv INPUTFILE OUTPUTFILE");
        return ExitCode::from(1);
    }
    let input_file = &args[1];
    let output_file = &args[2];

    println!("Reading file {input_file}...");
    let raw_bmp = fs::read(input_file).unwrap();

    match bmp::decode(&raw_bmp) {
        Ok(img) => {
            match bmp::encode(&img) {
                Ok(data) => {
                    fs::write(output_file, data).unwrap();
                    println!("{output_file} successfully written!");
                }
                Err(err) => {
                    println!("encoding error: {err}");
                    return ExitCode::from(1);
                }
            }
        }
        Err(err) => {
            println!("decoding error: {err}");
            return ExitCode::from(1);
        }
    }


    ExitCode::from(0)
}
