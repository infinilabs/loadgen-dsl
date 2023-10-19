example := 'examples/example.dsl'
just := quote(just_executable()) + ' --justfile=' + quote(justfile())

_default:
    @{{just}} --list

# Run WASM module in debug build
dev input=example: build
    go run main.go -p target/wasm32-unknown-unknown/debug/loadgen_dsl.wasm -i {{input}}

# Build WASM module
build:
    cargo build --target wasm32-unknown-unknown

# Run WASM module
run input=example: release
    go run main.go -p dist/loadgen_dsl.wasm -i {{input}}

# Update bundle WASM binary
release:
    cargo build --target wasm32-unknown-unknown --release
    @mkdir -p dist
    cp target/wasm32-unknown-unknown/release/loadgen_dsl.wasm dist
