use arrow_array::Array;
use arrow_ipc::reader::FileReader;
use parquet_variant::VariantBuilder;
use parquet_variant_compute::{unshred_variant, VariantArray};
use std::{env, fs, path::Path};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<_> = env::args().collect();
    if args.len() != 3 {
        return Err("usage: wireform-typed-variant-reference INPUT.arrow OUTPUT_DIRECTORY".into());
    }
    let reader = FileReader::try_new(fs::File::open(&args[1])?, None)?;
    let output = Path::new(&args[2]);
    fs::create_dir_all(output)?;
    let mut row = 0;
    for batch in reader {
        let batch = batch?;
        if batch.num_columns() != 1 {
            return Err("expected one Variant column".into());
        }
        let storage = batch.column(0);
        let array = VariantArray::try_new(storage.as_ref())?;
        // try_value alone does not merge the fallback object fields. Use the
        // real recursive reconstruction kernel before materializing each row.
        let full = unshred_variant(&array)?;
        for i in 0..batch.num_rows() {
            let prefix = output.join(format!("{row:06}"));
            if storage.is_null(i) {
                fs::write(prefix.with_extension("null"), [])?;
            } else {
                let value = full.try_value(i)?;
                fs::write(prefix.with_extension("debug"), format!("{value:?}"))?;
                let mut builder = VariantBuilder::new();
                builder.try_append_value(value)?;
                let (metadata, value) = builder.finish();
                fs::write(prefix.with_extension("meta"), metadata)?;
                fs::write(prefix.with_extension("value"), value)?;
            }
            row += 1;
        }
    }
    println!("reconstructed {row} rows with pinned arrow-rs");
    Ok(())
}
