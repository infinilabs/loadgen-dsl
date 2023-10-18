package main

import (
	"context"
	_ "embed"
	"flag"
	"fmt"
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
	alloc := mod.ExportedFunction("allocate")
	free := mod.ExportedFunction("deallocate")
	compile := mod.ExportedFunction("compile")

	// write input
	inputSize := uint32(len(input))
	ret, err = alloc.Call(ctx, uint64(inputSize))
	if err != nil {
		log.Panic(err)
	}
	inputPtr := ret[0]
	defer free.Call(ctx, inputPtr)
	_, inputAddr, _ := decode_ptr(inputPtr)
	mod.Memory().Write(inputAddr, input)

	// prepare memory for results
	ret, err = alloc.Call(ctx, uint64(4))
	if err != nil {
		log.Panic(err)
	}
	errorPtr := ret[0]
	defer free.Call(ctx, errorPtr)

	// compile input
	ret, err = compile.Call(ctx, inputPtr)
	if err != nil {
		log.Panic(err)
	}
	outputPtr := ret[0]
	defer free.Call(ctx, outputPtr)

	// read output
	errors, outputAddr, outputSize := decode_ptr(outputPtr)
	bytes, _ := mod.Memory().Read(outputAddr, outputSize)
	// check errors
	if errors {
		fmt.Printf("Error:\n%s", bytes)
	} else {
		fmt.Printf("Output:\n%s", bytes)

	}
}

func decode_ptr(ptr uint64) (errors bool, addr, size uint32) {
	const SIZE_MASK uint32 = (^uint32(0)) >> 1
	addr = uint32(ptr)
	size = uint32(ptr>>32) & SIZE_MASK
	errors = (ptr >> 63) != 0
	return
}
