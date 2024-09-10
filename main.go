// MIT License
//
// Copyright (C) INFINI Labs & INFINI LIMITED.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
package main

import (
	"context"
	_ "embed"
	E "errors"
	"flag"
	"fmt"
	"log"
	"os"

	wasm "github.com/tetratelabs/wazero"
	wasmAPI "github.com/tetratelabs/wazero/api"
)

var plugin = flag.String("p", "", "plugin path")
var input = flag.String("i", "", "input path")

func main() {
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

	// check output
	output, err := loadPlugins([][]byte{plugin}, string(input))
	if err != nil {
		fmt.Printf("#ERROR:\n%s", err)
	} else {
		fmt.Printf("#OUTPUT:\n%s", output)
	}
}

func loadPlugins(plugins [][]byte, input string) (output string, err error) {
	// init runtime
	ctx := context.Background()
	r := wasm.NewRuntime(ctx)
	defer r.Close(ctx)

	var mod wasmAPI.Module
	for _, plug := range plugins {
		// load plugin
		mod, err = r.Instantiate(ctx, plug)
		if err != nil {
			return
		}
		// call plugin
		output, err = callPlugin(ctx, mod, string(input))
		if err != nil {
			break
		}
	}
	return
}

func callPlugin(ctx context.Context, mod wasmAPI.Module, input string) (output string, err error) {
	init := mod.ExportedFunction("init")
	alloc := mod.ExportedFunction("allocate")
	free := mod.ExportedFunction("deallocate")
	process := mod.ExportedFunction("process")

	// initialize
	if init != nil {
		init.Call(ctx)
	}

	// write input
	inputSize := uint32(len(input))
	ret, err := alloc.Call(ctx, uint64(inputSize))
	if err != nil {
		return
	}
	inputPtr := ret[0]
	defer free.Call(ctx, inputPtr)
	_, inputAddr, _ := decodePtr(inputPtr)
	mod.Memory().Write(inputAddr, []byte(input))

	// prepare memory for results
	ret, err = alloc.Call(ctx, uint64(4))
	if err != nil {
		return
	}
	errorPtr := ret[0]
	defer free.Call(ctx, errorPtr)

	// compile input
	ret, err = process.Call(ctx, inputPtr)
	if err != nil {
		return
	}
	outputPtr := ret[0]
	defer free.Call(ctx, outputPtr)

	// read output
	errors, outputAddr, outputSize := decodePtr(outputPtr)
	bytes, _ := mod.Memory().Read(outputAddr, outputSize)

	if errors {
		err = E.New(string(bytes))
	} else {
		output = string(bytes)
	}
	return
}

func decodePtr(ptr uint64) (errors bool, addr, size uint32) {
	const SIZE_MASK uint32 = (^uint32(0)) >> 1
	addr = uint32(ptr)
	size = uint32(ptr>>32) & SIZE_MASK
	errors = (ptr >> 63) != 0
	return
}
