// Independent fixture oracle: decode with the unmodified Apache Arrow C++
// library shipped by PyArrow. Keys below are PUBLIC parquet-testing fixtures.
#include <array>
#include <bit>
#include <cstdint>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string_view>
#include <unordered_map>
#include "arrow/filesystem/localfs.h"
#include "arrow/io/file.h"
#include "arrow/ipc/api.h"
#include "arrow/table.h"
#include "arrow/util/secure_string.h"
#include "parquet/arrow/reader.h"
#include "parquet/bloom_filter.h"
#include "parquet/bloom_filter_reader.h"
#include "parquet/column_reader.h"
#include "parquet/encryption/crypto_factory.h"
#include "parquet/encryption/kms_client_factory.h"
#include "parquet/file_reader.h"
#include "parquet/page_index.h"

// This exported library helper implements Apache's mock InMemoryKMS wrapping.
// Its declaration matches parquet/encryption/key_toolkit_internal.h; that
// internal header is not distributed in PyArrow's public include directory.
namespace parquet::encryption::internal {
::arrow::util::SecureString DecryptKeyLocally(const std::string&,
    const ::arrow::util::SecureString&, const std::string&);
}
using arrow::util::SecureString;
using Keys = std::unordered_map<std::string, SecureString>;

Keys FixtureKeys(bool wide) {
  Keys keys;
  keys.emplace("kf", SecureString(std::string(wide ? "01234567890123456789012345678901" : "0123456789012345")));
  for (int n = 1; n <= (wide ? 8 : 2); ++n) {
    auto key = wide ? "123456789012345678901234567890" + std::to_string(n + 11)
                    : "123456789012345" + std::to_string(n - 1);
    keys.emplace("kc" + std::to_string(n), SecureString(std::move(key)));
  }
  return keys;
}

class FixtureKms : public parquet::encryption::KmsClient {
  Keys keys_ = FixtureKeys(false);
 public:
  std::string WrapKey(const SecureString&, const std::string&) override {
    throw std::runtime_error("read-only fixture KMS");
  }
  SecureString UnwrapKey(const std::string& wrapped, const std::string& id) override {
    if (id == "wireform-test" && wrapped == "test-key") return SecureString(16, 0x42);
    if (id == "wireform-test") {
      for (int bytes : {16,24,32}) {
        if (wrapped == "test-key-" + std::to_string(bytes)) return SecureString(bytes, 0x42);
      }
    }
    return parquet::encryption::internal::DecryptKeyLocally(wrapped, keys_.at(id), id);
  }
};
class FixtureKmsFactory : public parquet::encryption::KmsClientFactory {
 public:
  std::shared_ptr<parquet::encryption::KmsClient> CreateKmsClient(
      const parquet::encryption::KmsConnectionConfig&) override {
    return std::make_shared<FixtureKms>();
  }
};

std::string Hex(const void* raw, size_t size) {
  const auto* data = static_cast<const uint8_t*>(raw);
  constexpr char digits[] = "0123456789abcdef";
  std::string out; out.reserve(size * 2);
  for (size_t i = 0; i < size; ++i) {
    out += digits[data[i] >> 4]; out += digits[data[i] & 15];
  }
  return out;
}

std::string JsonString(const std::string& text) {
  constexpr char hex[] = "0123456789abcdef";
  std::string out = "\"";
  for (uint8_t byte : text) {
    if (byte == '"' || byte == '\\') { out += '\\'; out += byte; }
    else if (byte < 32) { out += "\\u00"; out += hex[byte >> 4]; out += hex[byte & 15]; }
    else out += byte;
  }
  return out + '"';
}
template<class T> std::string PlainValue(T value, int) {
  static_assert(std::endian::native == std::endian::little);
  return std::string(reinterpret_cast<const char*>(&value), sizeof(T));
}
template<> std::string PlainValue(bool value, int) {
  return std::string(1, value ? '\1' : '\0');
}
template<> std::string PlainValue(parquet::ByteArray value, int) {
  return value.len ? std::string(reinterpret_cast<const char*>(value.ptr), value.len) : std::string();
}
template<> std::string PlainValue(parquet::FixedLenByteArray value, int width) {
  return std::string(reinterpret_cast<const char*>(value.ptr), width);
}

template<class Type> void Dump(parquet::ColumnReader* raw, std::ostream& out, const parquet::BloomFilter* bloom) {
  auto* reader = dynamic_cast<parquet::TypedColumnReader<Type>*>(raw);
  if (!reader) throw std::runtime_error("physical reader mismatch");
  const auto* schema = reader->descr();
  using T = typename Type::c_type;
  std::array<T, 128> values;
  std::array<int16_t, 128> defs{}, reps{};
  std::ostringstream definitions, repetitions, payloads;
  bool first_level = true, first_value = true;
  int64_t events = 0, present = 0;
  while (reader->HasNext()) {
    int64_t count = 0;
    auto levels = reader->ReadBatch(values.size(), defs.data(), reps.data(), values.data(), &count);
    if (levels <= 0) throw std::runtime_error("reader made no progress");
    for (int64_t i = 0; i < levels; ++i) {
      if (!first_level) { definitions << ','; repetitions << ','; }
      first_level = false;
      definitions << (schema->max_definition_level() ? defs[i] : 0);
      repetitions << (schema->max_repetition_level() ? reps[i] : 0);
    }
    for (int64_t i = 0; i < count; ++i) {
      if (!first_value) payloads << ',';
      first_value = false;
      const auto plain = PlainValue(values[i], schema->type_length());
      if (bloom && !bloom->FindHash(bloom->Hash(std::string_view(plain)))) {
        throw std::runtime_error("bloom false negative for an independently decoded value");
      }
      payloads << '\"' << Hex(plain.data(), plain.size()) << '\"';
    }
    events += levels; present += count;
  }
  out << ",\"bloomValuesChecked\":" << (bloom ? present : 0)
      << ",\"events\":" << events << ",\"present\":" << present
      << ",\"definitions\":[" << definitions.str() << "]"
      << ",\"repetitions\":[" << repetitions.str() << "]"
      << ",\"values\":[" << payloads.str() << "]";
}

int main(int argc, char** argv) {
 try {
  if (argc != 2 && argc != 3 && argc != 4) throw std::runtime_error("usage: reference <encrypted fixture> [arrow-output [native-forward|plaintext]]");
  const bool native_forward = argc == 4 && std::string(argv[3]) == "native-forward";
  const bool plaintext = argc == 4 && std::string(argv[3]) == "plaintext";
  const bool writer_features = argc == 4 && std::string(argv[3]).starts_with("writer-");
  if (argc == 4 && !native_forward && !plaintext && !writer_features) throw std::runtime_error("unknown fixture key configuration");
  const std::string path = argv[1];
  parquet::ReaderProperties properties;
  properties.set_page_checksum_verification(true);
  std::shared_ptr<parquet::FileDecryptionProperties> decryption;
  const bool external = path.find("external_key_material") != std::string::npos;
  if (writer_features) {
    const int width = std::stoi(std::string(argv[3]).substr(7));
    if (width != 16 && width != 24 && width != 32) throw std::runtime_error("invalid public fixture key width");
    auto retriever = std::make_shared<parquet::StringKeyIdRetriever>();
    retriever->PutKey("kf", SecureString(width, 0x42));
    retriever->PutKey("kc1", SecureString(width, 0x43));
    retriever->PutKey("kc2", SecureString(width, 0x44));
    parquet::FileDecryptionProperties::Builder builder;
    builder.key_retriever(retriever);
    builder.aad_prefix("writer-regression");
    decryption = builder.build();
  } else if (external || native_forward) {
    parquet::encryption::CryptoFactory factory;
    factory.RegisterKmsClientFactory(std::make_shared<FixtureKmsFactory>());
    parquet::encryption::KmsConnectionConfig connection;
    if (native_forward) connection.kms_instance_id = "wireform-test";
    parquet::encryption::DecryptionConfiguration config;
    decryption = factory.GetFileDecryptionProperties(connection, config, path,
        std::make_shared<arrow::fs::LocalFileSystem>());
  } else {
    auto retriever = std::make_shared<parquet::StringKeyIdRetriever>();
    for (const auto& [id, key] : FixtureKeys(path.find("aes256/") != std::string::npos)) {
      retriever->PutKey(id, key);
    }
    parquet::FileDecryptionProperties::Builder builder;
    builder.key_retriever(retriever);
    if (path.find("_aad") != std::string::npos) builder.aad_prefix("tester");
    decryption = builder.build();
  }
  if (!plaintext) properties.file_decryption_properties(decryption);
  auto file = parquet::ParquetFileReader::OpenFile(path, false, properties);
  auto metadata = file->metadata();
  std::ifstream input(path, std::ios::binary);
  const std::string raw((std::istreambuf_iterator<char>(input)), std::istreambuf_iterator<char>());
  if (raw.size() < 12) throw std::runtime_error("short fixture");
  auto length = static_cast<uint32_t>(static_cast<uint8_t>(raw[raw.size()-8]))
      | static_cast<uint32_t>(static_cast<uint8_t>(raw[raw.size()-7])) << 8
      | static_cast<uint32_t>(static_cast<uint8_t>(raw[raw.size()-6])) << 16
      | static_cast<uint32_t>(static_cast<uint8_t>(raw[raw.size()-5])) << 24;
  if (length > raw.size()-12) throw std::runtime_error("fixture footer out of bounds");
  parquet::EncryptionAlgorithm algorithm{};
  std::string footer_key_metadata;
  if (raw.substr(raw.size()-4) == "PARE") {
    auto crypto = parquet::FileCryptoMetaData::Make(
        reinterpret_cast<const uint8_t*>(raw.data()+raw.size()-8-length), static_cast<int64_t>(length));
    algorithm = crypto->encryption_algorithm(); footer_key_metadata = crypto->key_metadata();
  } else if (!plaintext) {
    algorithm = metadata->encryption_algorithm(); footer_key_metadata = metadata->footer_signing_key_metadata();
  }
  auto footer_key = decryption->key_retriever()->GetKey(plaintext ? "kf" : footer_key_metadata);
  auto indexes = file->GetPageIndexReader();
  std::cout << "{\"rows\":" << metadata->num_rows() << ",\"rowGroups\":"
            << metadata->num_row_groups() << ",\"columns\":" << metadata->num_columns()
            << ",\"fileIdHex\":\"" << Hex(algorithm.aad.aad_file_unique.data(), algorithm.aad.aad_file_unique.size())
            << "\",\"storedPrefixHex\":\"" << Hex(algorithm.aad.aad_prefix.data(), algorithm.aad.aad_prefix.size())
            << "\",\"prefixRequired\":" << (algorithm.aad.supply_aad_prefix ? "true" : "false")
            << ",\"algorithm\":" << algorithm.algorithm
            << ",\"footerKeyMetadataHex\":\"" << Hex(footer_key_metadata.data(), footer_key_metadata.size()) << "\""
            << ",\"publicFixtureFooterKeyHex\":\"" << Hex(footer_key.as_span().data(), footer_key.size()) << "\"}\n";
  for (int rg = 0; rg < metadata->num_row_groups(); ++rg) {
    auto group = file->RowGroup(rg);
    auto rg_indexes = indexes ? indexes->RowGroup(rg) : nullptr;
    auto rg_blooms = file->GetBloomFilterReader().RowGroup(rg);
    for (int col = 0; col < metadata->num_columns(); ++col) {
      auto reader = group->Column(col);
      auto* schema = reader->descr();
      auto ci = rg_indexes ? rg_indexes->GetColumnIndex(col) : nullptr;
      auto oi = rg_indexes ? rg_indexes->GetOffsetIndex(col) : nullptr;
      auto bloom = rg_blooms ? rg_blooms->GetColumnBloomFilter(col) : nullptr;
      auto crypto = metadata->RowGroup(rg)->ColumnChunk(col)->crypto_metadata();
      std::string key_metadata;
      SecureString column_key;
      if (crypto) {
        key_metadata = crypto->key_metadata();
        column_key = crypto->encrypted_with_footer_key() ? footer_key : decryption->key_retriever()->GetKey(key_metadata);
      }
      std::cout << "{\"rowGroup\":" << rg << ",\"column\":" << col
        << ",\"path\":\"" << schema->path()->ToDotString() << "\",\"type\":" << schema->physical_type()
        << ",\"maxDefinition\":" << schema->max_definition_level()
        << ",\"maxRepetition\":" << schema->max_repetition_level()
        << ",\"columnIndex\":" << (ci ? "true" : "false")
        << ",\"offsetPages\":" << (oi ? oi->page_locations().size() : 0)
        << ",\"bloomBytes\":" << (bloom ? bloom->GetBitsetSize() : 0);
      std::cout << ",\"encrypted\":" << (crypto ? "true" : "false")
        << ",\"keyMetadataHex\":\"" << Hex(key_metadata.data(), key_metadata.size())
        << "\",\"publicFixtureColumnKeyHex\":\"" << Hex(column_key.as_span().data(), column_key.size()) << "\",\"pathParts\":[";
      bool first_part = true;
      const auto schema_path = schema->path();
      for (const auto& part : schema_path->ToDotVector()) {
        if (!first_part) std::cout << ',';
        first_part = false;
        std::cout << JsonString(part);
      }
      std::cout << ']';
      std::cout << ",\"pageLocations\":[";
      bool first_page = true;
      if (oi) for (const auto& location : oi->page_locations()) {
        if (!first_page) std::cout << ',';
        first_page = false;
        std::cout << '[' << location.offset << ',' << location.compressed_page_size << ',' << location.first_row_index << ']';
      }
      std::cout << "],\"pageMaxima\":[";
      first_page = true;
      if (ci) for (const auto& value : ci->encoded_max_values()) {
        if (!first_page) std::cout << ',';
        first_page = false;
        std::cout << '"' << Hex(value.data(), value.size()) << '"';
      }
      std::cout << "],\"nullPages\":[";
      first_page = true;
      if (ci) for (bool value : ci->null_pages()) {
        if (!first_page) std::cout << ',';
        first_page = false;
        std::cout << (value ? "true" : "false");
      }
      std::cout << ']';
      switch (schema->physical_type()) {
        case parquet::Type::BOOLEAN: Dump<parquet::BooleanType>(reader.get(), std::cout, bloom.get()); break;
        case parquet::Type::INT32: Dump<parquet::Int32Type>(reader.get(), std::cout, bloom.get()); break;
        case parquet::Type::INT64: Dump<parquet::Int64Type>(reader.get(), std::cout, bloom.get()); break;
        case parquet::Type::INT96: Dump<parquet::Int96Type>(reader.get(), std::cout, bloom.get()); break;
        case parquet::Type::FLOAT: Dump<parquet::FloatType>(reader.get(), std::cout, bloom.get()); break;
        case parquet::Type::DOUBLE: Dump<parquet::DoubleType>(reader.get(), std::cout, bloom.get()); break;
        case parquet::Type::BYTE_ARRAY: Dump<parquet::ByteArrayType>(reader.get(), std::cout, bloom.get()); break;
        case parquet::Type::FIXED_LEN_BYTE_ARRAY: Dump<parquet::FLBAType>(reader.get(), std::cout, bloom.get()); break;
        default: throw std::runtime_error("unsupported physical type");
      }
      std::cout << "}\n";
    }
  }
  if (argc >= 3 && std::string(argv[2]) != "-") {
    // Keep raw INT96 comparisons in the physical oracle: converting the
    // deliberately extreme fixture values to Arrow timestamps can overflow.
    std::vector<int> columns;
    for (int i = 0; i < metadata->num_columns(); ++i) {
      if (metadata->schema()->Column(i)->physical_type() != parquet::Type::INT96) columns.push_back(i);
    }
    auto reader = parquet::arrow::FileReader::Make(arrow::default_memory_pool(), std::move(file)).ValueOrDie();
    auto table = reader->ReadTable(columns).ValueOrDie();
    auto sink = arrow::io::FileOutputStream::Open(argv[2]).ValueOrDie();
    auto output = arrow::ipc::MakeStreamWriter(sink, table->schema()).ValueOrDie();
    PARQUET_THROW_NOT_OK(output->WriteTable(*table));
    PARQUET_THROW_NOT_OK(output->Close());
    PARQUET_THROW_NOT_OK(sink->Close());
  }
  return 0;
 } catch (const std::exception& error) {
  std::cerr << error.what() << '\n'; return 1;
 }
}
