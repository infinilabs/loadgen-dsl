# How to build

```
rustup default nightly
cargo +nightly build -Z sparse-registry
rustup target add wasm32-unknown-unknown
rustup update
cargo clean
cargo build --target wasm32-unknown-unknown --release
```