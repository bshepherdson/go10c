package main

import (
	"fmt"
	"sort"
)

type DCPU bool

var dcpuRegs = [8]string{"A", "B", "C", "X", "Y", "Z", "I", "J"}

func (d *DCPU) Reg(i int) string {
	return dcpuRegs[i]
}

func (d *DCPU) RegCount() int {
	return 8
}

func (d *DCPU) UsableRegs() []int {
	return []int{0, 1, 2, 3, 4, 6, 7}
}

func (d *DCPU) FP() string { return "Z" }
func (d *DCPU) SP() string { return "SP" }
func (d *DCPU) PC() string { return "PC" }

func (d *DCPU) ReadFrame(reg, delta int) string {
	return fmt.Sprintf("set %s, [%s %+d]", d.Reg(reg), d.FP(), delta)
}

func (d *DCPU) WriteFrame(reg, delta int) string {
	return fmt.Sprintf("set [%s %+d], %s", d.FP(), delta, d.Reg(reg))
}

func (d *DCPU) Push(regs ...int) []string {
	sort.Ints(regs)
	ret := []string{}
	for i := len(regs) - 1; i >= 0; i-- {
		ret = append(ret, fmt.Sprintf("set push, %s", d.Reg(regs[i])))
	}
	return ret
}

func (d *DCPU) Pop(regs ...int) []string {
	sort.Ints(regs)
	ret := []string{}
	for _, r := range regs {
		ret = append(ret, fmt.Sprintf("set %s, pop", d.Reg(r)))
	}
	return ret
}

func (d *DCPU) Store(from, to *Location, c *Compiler) {
	if to.Kind == locConstant {
		return // Nothing to do.
	}
	// Otherwise, on the DCPU, it's just a SET operation with the right arguments.
	c.compile(dBin("set", to.AsDArg(), from.AsDArg()))
}

// DCPU assembly output.
type DAsm interface {
	Outputtable
}

type DOp struct {
	op      string
	a       DArg
	b       DArg
	comment string // Useful for debugging.
}

type DComment string
type DLabel string

// Uninitialized data.
type DReserve struct {
	size int
}

// .dat statements are DOps with op = ".dat"

// DCPU assembly arguments.
type DArg interface {
	Outputtable
	Static() bool // True if this value is static and safe to put in a .dat
}

type DAReg struct {
	reg      int
	indirect bool
}

type DALit struct {
	lit      int
	indirect bool
}

type DARegLit struct {
	reg, lit int
	// Always indirect.
}

type DAFrameOffset struct {
	delta int
}

type DALabel struct {
	label    string
	indirect bool
}

type DASpecial struct {
	text string
}

const (
	dasPeek = "peek"
	dasSP   = "sp"
	dasPC   = "pc"
	dasEX   = "ex"
)

type DExprResult struct {
	result DArg
}

func (r *DExprResult) Set(x interface{}) {
	r.result = x.(DArg)
}

type Outputtable interface {
	Output() string
}

func (a DComment) Output() string {
	return "; " + string(a)
}

func (a DLabel) Output() string {
	return ":" + string(a)
}

func (a *DOp) Output() string {
	s := "  " + a.op
	if a.b != nil {
		s += " " + a.b.Output() + ","
	}
	s += " " + a.a.Output()
	if a.comment != "" {
		s += "    ; " + a.comment
	}
	return s
}

func (a *DReserve) Output() string {
	return fmt.Sprintf(".reserve %d", a.size)
}

func (a *DAReg) Output() string {
	if a.indirect {
		return "[" + dcpuRegs[a.reg] + "]"
	} else {
		return dcpuRegs[a.reg]
	}
}

func (a *DALit) Output() string {
	s := fmt.Sprintf("%d", a.lit)
	if a.indirect {
		return "[" + s + "]"
	}
	return s
}

func (a *DALabel) Output() string {
	if a.indirect {
		return "[" + a.label + "]"
	}
	return a.label
}

func (a *DARegLit) Output() string {
	return fmt.Sprintf("[%s %+d]", dcpuRegs[a.reg], a.lit)
}

func (a *DASpecial) Output() string {
	return a.text
}

func (a *DAFrameOffset) Output() string {
	return fmt.Sprintf("[SP %+d]", a.delta)
}

func (a *DAReg) Static() bool         { return false }
func (a *DALit) Static() bool         { return true }
func (a *DARegLit) Static() bool      { return false }
func (a *DASpecial) Static() bool     { return false }
func (a *DAFrameOffset) Static() bool { return false }
func (a *DALabel) Static() bool       { return !a.indirect }

// Helper functions for building different kinds of ops.

// Written in assembler order: op b, a
func dBin(op string, b, a DArg) DAsm {
	return &DOp{op: op, a: a, b: b}
}

func dUnary(op string, a DArg) DAsm {
	return &DOp{op: op, a: a}
}

func dLabel(label string) DAsm {
	return DLabel(label)
}
func dComment(comment string) DAsm {
	return DComment(comment)
}

func dPush(a DArg) DAsm {
	return dBin("set", &DASpecial{"push"}, a)
}

func dPop(b DArg) DAsm {
	return dBin("set", b, &DASpecial{"pop"})
}

func daReg(reg int) DArg {
	return &DAReg{reg: reg}
}

func daAddrReg(reg int) DArg {
	return &DAReg{reg: reg, indirect: true}
}

func daLit(lit int) DArg {
	return &DALit{lit: lit}
}

func daAddrLit(lit int) DArg {
	return &DALit{lit: lit, indirect: true}
}

func daAddrRegLit(reg, lit int) DArg {
	return &DARegLit{reg: reg, lit: lit}
}

func daSP() DArg {
	return &DASpecial{"SP"}
}
func daFP() DArg {
	return daReg(5)
}
func daPC() DArg {
	return &DASpecial{"PC"}
}
