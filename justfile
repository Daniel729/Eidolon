CARGO_ROOT := x'${CARGO_TARGET_DIR:-target}'

make-all:
    rm -rf bin
    mkdir bin
    just build-linux-gnu
    just build-windows

build-linux-gnu:
    RUSTFLAGS="" cargo +nightly build --profile=release-lto --bin eidolon
    cp {{CARGO_ROOT}}/release-lto/eidolon ./bin/eidolon_x86_64-unknown-linux-gnu

build-windows:
    RUSTFLAGS="" cargo +nightly build --profile=release-lto --bin eidolon --target=x86_64-pc-windows-gnu 
    cp {{CARGO_ROOT}}/x86_64-pc-windows-gnu/release-lto/eidolon.exe ./bin/eidolon_x86_64-pc-windows-gnu.exe