//! Reference interop check: fully materialize one user value, then write it
//! as Ion 1.0 binary. Input and output paths are explicit; no network or IO
//! takes place within either codec benchmark.
use ion_rs::{v1_0::Binary, Element};
use std::{env, fs};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<_> = env::args().collect();
    if args.len() != 3 {
        return Err("usage: wireform-ion-rs-interop INPUT OUTPUT".into());
    }
    let input = fs::read(&args[1])?;
    let element = Element::read_one(input)?;
    let output: Vec<u8> = element.encode_as(Binary)?;
    assert_eq!(Element::read_one(&output)?, element);
    fs::write(&args[2], output)?;
    Ok(())
}
