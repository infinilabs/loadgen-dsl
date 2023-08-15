package main

import (
	"context"
	_ "embed"
	"io/ioutil"
	"log"

	"github.com/tetratelabs/wazero"
)

//go:embed target/wasm32-unknown-unknown/release/loadgen_dsl_wasm.wasm
var wasm []byte

func main() {
	var ret []uint64
	var err error

	// read arguments
	path := "examples/example.dsl"
	// path := "example/example.dsl"
	input, err := ioutil.ReadFile(path)
	if err != nil {
		log.Panicln(err)
	}

	// init runtime
	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)

	// load module
	mod, err := r.Instantiate(ctx, wasm)
	if err != nil {
		log.Panicln(err)
	}
	alloc := mod.ExportedFunction("alloc")
	free := mod.ExportedFunction("free")
	compile := mod.ExportedFunction("compile")

	// write input
	inputSize := uint32(len(input))
	ret, err = alloc.Call(ctx, uint64(inputSize))
	if err != nil {
		log.Panic(err)
	}
	inputPtr := ret[0]
	defer free.Call(ctx, inputPtr)
	inputOffset, _ := decode_ptr(inputPtr)
	mod.Memory().Write(inputOffset, input)

	// compile input
	ret, err = compile.Call(ctx, inputPtr)
	if err != nil {
		log.Panic(err)
	}
	outputPtr := ret[0]
	defer free.Call(ctx, outputPtr)

	// read output
	bytes, _ := mod.Memory().Read(decode_ptr(outputPtr))
	println(string(bytes))
}

func decode_ptr(ptr uint64) (uint32, uint32) {
	return uint32(ptr), uint32(ptr >> 32)
}

func encode_ptr(ptr uint32, size int) uint64 {
	return uint64(ptr) | uint64(size)<<32
}
