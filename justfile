just := quote(just_executable()) + ' --justfile=' + quote(justfile())

_default:
    @{{just}} --list

# Build WASM module
build:
    cargo build --target wasm32-unknown-unknown --release
    @mkdir -p dist
    cp target/wasm32-unknown-unknown/release/loadgen_dsl.wasm dist

# Run WASM module with input
run input='examples/example.dsl': build
    go run main.go -p dist/loadgen_dsl.wasm -i {{input}}
