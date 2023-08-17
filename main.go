package main

import (
	"context"
	_ "embed"
	"flag"
	"log"
	"os"

	"github.com/tetratelabs/wazero"
)

var plugin = flag.String("p", "", "plugin path")
var input = flag.String("i", "", "input path")

func main() {
	var ret []uint64
	var err error

	flag.Parse()
	if len(*plugin) == 0 || len(*input) == 0 {
		flag.Usage()
		os.Exit(1)
	}

	// read arguments
	plugin, err := os.ReadFile(*plugin)
	if err != nil {
		log.Panicln(err)
	}
	input, err := os.ReadFile(*input)
	if err != nil {
		log.Panicln(err)
	}

	// init runtime
	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)

	// load module
	mod, err := r.Instantiate(ctx, plugin)
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
