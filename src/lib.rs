/// Set the global allocator to the WebAssembly optimized one.
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[export_name = "alloc"]
pub unsafe extern "C" fn _alloc(size: usize) -> u64 {
    encode_ptr(
        Box::into_raw(Vec::<u8>::with_capacity(size).into_boxed_slice()) as _,
        size,
    )
}

#[export_name = "free"]
pub unsafe extern "C" fn _free(ptr: u64) {
    let (ptr, size) = decode_ptr(ptr);
    let _ = Box::from_raw(std::slice::from_raw_parts_mut(ptr as *mut u8, size as _).as_mut_ptr());
}

#[export_name = "compile"]
pub unsafe extern "C" fn _compile(ptr: u64) -> u64 {
    let (ptr, size) = decode_ptr(ptr);
    let input = std::str::from_utf8_unchecked(std::slice::from_raw_parts(ptr, size));
    let output = Box::into_raw(compile(input).into_bytes().into_boxed_slice());
    encode_ptr(output as _, (*output).len())
}

fn decode_ptr(ptr: u64) -> (*mut u8, usize) {
    (ptr as u32 as _, (ptr >> 32) as _)
}

fn encode_ptr(ptr: *mut u8, size: usize) -> u64 {
    (ptr as u64) | (size as u64) << 32
}

fn compile(input: &str) -> String {
    let config = loadgen_dsl_compiler::Compiler::new()
        .compile(input)
        .unwrap();
    serde_yaml::to_string(&config).expect("should be a invalid configuration")
}
