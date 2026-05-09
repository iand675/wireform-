#!/usr/bin/env bash
set -euo pipefail

UNICODE_VERSION="15.0.0"
DATA_DIR="data/unicode"

echo "=== Downloading Unicode Data for IDNA2008 ==="
echo "Unicode Version: ${UNICODE_VERSION}"
echo "Target Directory: ${DATA_DIR}"
echo

# Create data directory
mkdir -p "${DATA_DIR}"

# Note: IDNA2008 code point status is derived algorithmically from Unicode properties
# per RFC 5892, not from a static table. We only need UCD files.

# Download Unicode Character Database files
UCD_BASE="https://www.unicode.org/Public/${UNICODE_VERSION}/ucd"

echo "[1/4] Downloading DerivedBidiClass.txt..."
curl -fsSL -o "${DATA_DIR}/DerivedBidiClass.txt" \
  "${UCD_BASE}/extracted/DerivedBidiClass.txt"
echo "  ✓ Saved to ${DATA_DIR}/DerivedBidiClass.txt"

echo "[2/4] Downloading UnicodeData.txt..."
curl -fsSL -o "${DATA_DIR}/UnicodeData.txt" \
  "${UCD_BASE}/UnicodeData.txt"
echo "  ✓ Saved to ${DATA_DIR}/UnicodeData.txt"

echo "[3/4] Downloading DerivedJoiningType.txt..."
curl -fsSL -o "${DATA_DIR}/DerivedJoiningType.txt" \
  "${UCD_BASE}/extracted/DerivedJoiningType.txt"
echo "  ✓ Saved to ${DATA_DIR}/DerivedJoiningType.txt"

echo "[4/4] Downloading Scripts.txt..."
curl -fsSL -o "${DATA_DIR}/Scripts.txt" \
  "${UCD_BASE}/Scripts.txt"
echo "  ✓ Saved to ${DATA_DIR}/Scripts.txt"

echo
echo "✓ All Unicode data downloaded successfully!"
echo "  Files saved to: ${DATA_DIR}/"
echo "  Total size: $(du -sh ${DATA_DIR} 2>/dev/null | cut -f1 || echo 'N/A')"
echo
echo "Next steps:"
echo "  1. Commit these files: git add ${DATA_DIR}/"
echo "  2. Build the project: stack build"

