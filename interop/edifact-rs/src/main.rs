//! Read using the pinned reference parser and rewrite with default advice.
//! This intentionally normalizes service characters while preserving values.
use edifact_rs::{from_bytes, segments_to_bytes};
use std::{env, fs};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<_> = env::args().collect();
    if args.len() != 3 {
        return Err("usage: wireform-edifact-rs-reference INPUT OUTPUT".into());
    }
    let input = fs::read(&args[1])?;
    let segments = from_bytes(&input).collect::<Result<Vec<_>, _>>()?;
    let encoded = segments_to_bytes(&segments)?;
    fs::write(&args[2], encoded)?;
    Ok(())
}
