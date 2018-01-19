package main

// Code generation internals.

// CPU abstracts some of the differences between the different CPUs.
type CPU interface {
	Reg(int) string
	UsableRegs() []int
	RegCount() int
	FP() string
	SP() string
	PC() string
	ReadFrame(reg, delta int) string  // Indexes off the frame pointer.
	WriteFrame(reg, delta int) string // Indexes off the frame pointer.
	Store(from, to *Location, c *Compiler)

	// These are ordered with the lower numbered registers in lower addresses.
	Push(...int) []string
	Pop(...int) []string
}

// Data form a stack of copies, which might differ. The bottom-most one
// (parent is nil) is considered the canonical location.
type Data struct {
	parent *Data
	dirty  bool
	loc    *Location
}

const (
	locReg = iota
	locConstant
	locLabel
	locFrame
	locStack
)

type Location struct {
	Kind  int
	n     int
	label string
}

func label(label string) *Data {
	return &Data{loc: &Location{Kind: locLabel, label: label}}
}

func reg(reg int) *Data {
	return &Data{loc: &Location{Kind: locReg, n: reg}}
}

func frame(delta int) *Data {
	return &Data{loc: &Location{Kind: locFrame, n: delta}}
}
