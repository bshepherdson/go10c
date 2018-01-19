package main

import (
	"fmt"
	"log"
)

// Register allocator.
// TODO: Sophisticated replacement. This is a naive greedy allocator that
// expects things to get saved to memory consistently.

const (
	rFree = iota
	rReserved
	rUsed
)

type Allocator struct {
	regs    []int
	peak    int // Maximum number of registers saved.
	current int // Current number, for convenience.
	String  func(int) string
}

func newAllocator(count int) *Allocator {
	a := new(Allocator)
	a.regs = make([]int, count)
	return a
}

func (a *Allocator) Init(reserved int) {
	for i := 0; i < reserved; i++ {
		a.regs[i] = rReserved
	}
	for i := reserved; i < len(a.regs); i++ {
		a.regs[i] = rFree
	}
	a.peak = reserved
	a.current = reserved
}

func (a *Allocator) Get() int {
	for i, state := range a.regs {
		if state == rFree {
			a.regs[i] = rUsed
			a.bump()
			return i
		}
	}
	log.Fatal("Ran out of registers!")
	return -1
}

func (a *Allocator) Reserve(i int) bool {
	if a.regs[i] == rFree {
		a.regs[i] = rReserved
		a.bump()
		return true
	}
	fmt.Printf("WARN: Could not reserve %s", a.String(i))
	return false
}

func (a *Allocator) Release(i int) {
	if a.regs[i] != rFree {
		a.current--
	}
	a.regs[i] = rFree
}

func (a *Allocator) bump() {
	a.current++
	if a.current > a.peak {
		a.peak = a.current
	}
}
