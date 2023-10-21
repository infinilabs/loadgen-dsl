use anyhow::Result;

mod fii;

/// Set the global allocator to the WebAssembly optimized one.
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

/// # Safety
///
/// `ptr` must be a valid UTF-8 string.
#[export_name = "process"]
unsafe extern "C" fn _process(ptr: u64) -> u64 {
    fii::run_with(|| {
        let bytes = fii::decode_ptr(ptr).unwrap();
        let config = std::str::from_utf8_unchecked(bytes);
        compile(config)
    })
}

fn compile(input: &str) -> Result<String> {
    Ok(serde_yaml::to_string(&loadgen_dsl_compiler::compile(
        input,
    )?)?)
}
