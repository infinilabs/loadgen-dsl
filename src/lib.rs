// MIT License
//
// Copyright (C) INFINI Labs & INFINI LIMITED.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
    let output = loadgen_dsl_compiler::compile_requests(input)?;
    Ok(serde_yaml::to_string(&output).unwrap())
}
