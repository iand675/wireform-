use parquet_variant::{Variant, VariantBuilder};
use std::{env, fs, path::Path};
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<_> = env::args().collect();
    let root = Path::new(&args[1]);
    for entry in fs::read_dir(root)? {
        let path = entry?.path();
        if path.extension().and_then(|x| x.to_str()) != Some("meta") {
            continue;
        }
        let metadata = fs::read(&path)?;
        let value = fs::read(path.with_extension("value"))?;
        match Variant::try_new(&metadata, &value) {
            Ok(v) => {
                let debug = format!("{:?}", v);
                let mut builder = VariantBuilder::new();
                builder.try_append_value(v)?;
                let (m, b) = builder.finish();
                fs::write(path.with_extension("rust-meta"), m)?;
                fs::write(path.with_extension("rust-value"), b)?;
                fs::write(path.with_extension("rust-status"), format!("OK {}", debug))?;
            }
            Err(e) => {
                fs::write(path.with_extension("rust-status"), format!("ERROR {}", e))?;
            }
        }
    }
    Ok(())
}
