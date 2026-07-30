# Verify that all expected encoding plugins are present in the built library.
#
# This test is designed to run both in cibuildwheel's test phase (validating
# shipped wheels) and locally via CTest. It ensures that the pygetdata wheels
# ship a consistent, minimum set of encoding plugins across all platforms.
#
# The expected encodings can be overridden via the GETDATA_EXPECTED_ENCODINGS
# environment variable (comma-separated list of encoding names, e.g.
# "bzip2,gzip,lzma,flac,zzip"). If unset, all five are required.

import os
import sys
import pygetdata

# Map human-readable names to pygetdata encoding constants
ENCODINGS = {
    "bzip2": pygetdata.BZIP2_ENCODED,
    "gzip":  pygetdata.GZIP_ENCODED,
    "lzma":  pygetdata.LZMA_ENCODED,
    "flac":  pygetdata.FLAC_ENCODED,
    "zzip":  pygetdata.ZZIP_ENCODED,
    "zstd":  pygetdata.ZSTD_ENCODED,
}

# Always-available built-in encodings (sanity check)
BUILTINS = {
    "sie":        pygetdata.SIE_ENCODED,
    "unencoded":  pygetdata.UNENCODED,
    "text":       pygetdata.TEXT_ENCODED,
}


def get_expected_encodings():
    """Return the set of encoding names we expect to be available."""
    env = os.environ.get("GETDATA_EXPECTED_ENCODINGS", "").strip()
    if env:
        return [e.strip() for e in env.split(",") if e.strip()]
    # Default: require all five plugin encodings
    return list(ENCODINGS.keys())


def main():
    errors = 0

    print(f"pygetdata version: {pygetdata.__version__}")
    print(f"Platform: {sys.platform}")
    print()

    # Sanity check: built-in encodings must always be present
    for name, const in BUILTINS.items():
        result = pygetdata.encoding_support(const)
        if result == pygetdata.RDWR:
            print(f"  [OK]   {name}: read-write")
        elif result == pygetdata.RDONLY:
            print(f"  [OK]   {name}: read-only")
        else:
            print(f"  [FAIL] {name}: not supported (unexpected)")
            errors += 1

    print()

    # Check plugin encodings
    expected = get_expected_encodings()
    print(f"Expected plugin encodings: {', '.join(expected)}")
    print()

    for name in expected:
        const = ENCODINGS[name]
        result = pygetdata.encoding_support(const)
        if result == pygetdata.RDWR:
            print(f"  [OK]   {name}: read-write")
        elif result == pygetdata.RDONLY:
            print(f"  [OK]   {name}: read-only")
        else:
            print(f"  [FAIL] {name}: NOT SUPPORTED")
            errors += 1

    # Also report on any non-expected encodings (informational)
    extras = set(ENCODINGS.keys()) - set(expected)
    if extras:
        print()
        print("Other plugin encodings (not required):")
        for name in sorted(extras):
            const = ENCODINGS[name]
            result = pygetdata.encoding_support(const)
            if result == pygetdata.RDWR:
                print(f"  [info] {name}: read-write")
            elif result == pygetdata.RDONLY:
                print(f"  [info] {name}: read-only")
            else:
                print(f"  [info] {name}: not supported")

    print()
    if errors:
        print(f"FAILED: {errors} encoding(s) missing")
        sys.exit(1)
    else:
        print("PASSED: all expected encodings available")
        sys.exit(0)


if __name__ == "__main__":
    main()
