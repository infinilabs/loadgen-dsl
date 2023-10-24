use miette::{MietteHandlerOpts, Result};

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

#[export_name = "init"]
extern "C" fn _init() {
    miette::set_hook(Box::new(|_| {
        Box::new(MietteHandlerOpts::new().color(false).unicode(true).build())
    }))
    .unwrap();
}

fn compile(input: &str) -> Result<String> {
    let output = loadgen_dsl_compiler::compile_requests(&input)?;
    Ok(serde_yaml::to_string(&output).unwrap())
}
