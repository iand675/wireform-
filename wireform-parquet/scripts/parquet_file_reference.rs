use std::{env, fs, fs::File, path::Path};
use arrow_array::RecordBatchReader;
use arrow_ipc::{reader::{FileReader, StreamReader}, writer::FileWriter};
use parquet::arrow::{ArrowWriter, arrow_reader::{ArrowReaderOptions, ParquetRecordBatchReaderBuilder}};
use parquet::file::properties::WriterProperties;

fn convert(mode: &str, input: &Path, output: &Path) -> Result<(), Box<dyn std::error::Error>> {
    if mode == "to-arrow" {
        let options = ArrowReaderOptions::new().with_skip_arrow_metadata(true);
        let reader = ParquetRecordBatchReaderBuilder::try_new_with_options(File::open(input)?, options)?.build()?;
        let schema = reader.schema().clone();
        let mut writer = FileWriter::try_new(File::create(output)?, &schema)?;
        for batch in reader { writer.write(&batch?)?; }
        writer.finish()?;
    } else if mode == "from-arrow" {
        let reader = FileReader::try_new(File::open(input)?, None)?;
        let mut writer = ArrowWriter::try_new(File::create(output)?, reader.schema(), Some(WriterProperties::builder().build()))?;
        for batch in reader { writer.write(&batch?)?; }
        writer.close()?;
    } else if mode == "from-stream" {
        let reader = StreamReader::try_new(File::open(input)?, None)?;
        let mut writer = ArrowWriter::try_new(File::create(output)?, reader.schema(), Some(WriterProperties::builder().build()))?;
        for batch in reader { writer.write(&batch?)?; }
        writer.close()?;
    } else { return Err("mode must be to-arrow, from-arrow or from-stream".into()); }
    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<_> = env::args().collect();
    if args.len() != 4 { return Err("usage: parquet-file-reference MODE INPUT_DIR OUTPUT_DIR".into()); }
    fs::create_dir_all(&args[3])?;
    let mut entries = fs::read_dir(&args[2])?.collect::<Result<Vec<_>, _>>()?;
    entries.sort_by_key(|entry| entry.path());
    for entry in entries {
        let input = entry.path();
        let wanted = if args[1] == "to-arrow" { "parquet" } else if args[1] == "from-stream" { "arrows" } else { "arrow" };
        if input.extension().and_then(|ext| ext.to_str()) != Some(wanted) { continue; }
        let extension = if args[1] == "to-arrow" { "arrow" } else { "parquet" };
        let output = Path::new(&args[3]).join(input.file_stem().ok_or("missing file stem")?).with_extension(extension);
        convert(&args[1], &input, &output)?;
        println!("OK {}", input.display());
    }
    Ok(())
}
