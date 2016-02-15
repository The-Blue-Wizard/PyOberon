#!/bin/python

# PyOberon 0.1 - Oberon 07 compiler (re-)written in Python
# Copyright (C) 2016  John "The Blue Wizard" Rogers

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


# PyOberon code generator for RISC 5

#  NW  31.5.2015  code generator in Oberon-07 for RISC

# # MODULE ORG;

# Note: The file naming convention here is this: codegen[CPU]{size}.py
#       where CPU is currently "r" for RISC 0/5 as originally written in
#       ORG.Mod, and "rv" for RISC-V/G, and "size" is either "32" (default;
#       can be omitted) and "64". There is no 64-bit version of RISC 0/5,
#       so there won't be a Python version for it.

# # IMPORT SYSTEM, Files, ORS, ORB;

import scanner, symbols

# Code generator for Oberon compiler for RISC 5 processor.
# Procedural interface to Parser OSP; result in array "code".
# Procedure Close writes code-files

# CONST
WordSize = 4
StkOrg0 = -64
VarOrg0 = 0  # for RISC-0 only
# dedicated registers
MT = 12
SB = 13
SP = 14
LNK = 15

# maxCode = 8000
maxStrx = 2400
maxTD = 120
C24 = 0x1000000

# internal item modes
Reg  = 10
RegI = 11
Cond = 12

# frequently used opcodes

U = 0x2000
V = 0x1000

Mov = 0
Lsl = 1
Asr = 2
Ror = 3
And = 4
Ann = 5
Ior = 6
Xor = 7
Add = 8
Sub = 9
Cmp = 9
Mul = 10
Div = 11
Fad = 12
Fsb = 13
Fml = 14
Fdv = 15

Ldr = 8
Str = 10

BR  = 0
BLR = 1
BC  = 2
BL  = 3
MI  = 0
PL  = 8
EQ  = 1
NE  = 9
LT  = 5
GE  = 13
LE  = 6
GT  = 14

# # TYPE Item* = RECORD
    # # mode*: INTEGER;
    # # type*: ORB.Type;
    # # a*, b*, r: LONGINT;
    # # rdo*: BOOLEAN  (*read only*)
# # END ;

# Item forms and meaning of fields:
# mode    r      a       b
# --------------------------------
# Const   -     value (proc adr)  (immediate value)
# Var     base   off     -             (direct adr)
# Par      -     off0    off1        (indirect adr)
# Reg    regno
# RegI   regno   off     -
# Cond  cond   Fchain  Tchain

# # VAR pc*, varsize: LONGINT;   (*program counter, data index*)
  # # tdx, strx: LONGINT;
  # # entry: LONGINT;   (*main entry point*)
  # # RH: LONGINT;  (*available registers R[0] ... R[H-1]*)
  # # curSB: LONGINT;  (*current static base in SB*)
  # # frame: LONGINT;  (*frame offset changed in SaveRegs and RestoreRegs*)
  # # fixorgP, fixorgD, fixorgT: LONGINT;   (*origins of lists of locations to be fixed up by loader*)
  # # check: BOOLEAN;  (*emit run-time checks*)
  # # version: INTEGER;  (* 0 = RISC-0, 1 = RISC-5 *)
    
  # # relmap: ARRAY 6 OF INTEGER;  (*condition codes for relations*)
  # # code: ARRAY maxCode OF LONGINT;
  # # data: ARRAY maxTD OF LONGINT;  (*type descriptors*)
  # # str: ARRAY maxStrx OF CHAR;

relmap = [1, 9, 5, 6, 14, 13]

# instruction assemblers according to formats

# # PROCEDURE Put0(op, a, b, c: LONGINT);
def Put0(op, a, b, c):
    # # BEGIN (*emit format-0 instruction*)
    # emit format-0 instruction
    # # code[pc] := ((a*10H + b) * 10H + op) * 10000H + c; INC(pc)
    code.append(((a*0x10 + b) * 0x10 + op) * 0x10000 + c)
# # END Put0;

# # PROCEDURE Put1(op, a, b, im: LONGINT);
def Put1(op, a, b, im):
    # # BEGIN (*emit format-1 instruction,  -10000H <= im < 10000H*)
    # emit format-1 instruction,  -0x10000 <= im < 0x10000
    # # IF im < 0 THEN INC(op, V) END ;
    if im < 0:
        op += V
    # # code[pc] := (((a+40H) * 10H + b) * 10H + op) * 10000H + (im MOD 10000H); INC(pc)
    code.append((((a+0x40) * 0x10 + b) * 0x10 + op) * 0x10000 + (im & 0xFFFF))
# END Put1;

# # PROCEDURE Put1a(op, a, b, im: LONGINT);
def Put1a(op, a, b, im):
    # # BEGIN (*same as Pu1, but with range test  -10000H <= im < 10000H*)
    # same as Put1, but with range test  -0x10000 <= im < 0x10000
    # # IF (im >= -10000H) & (im <= 0FFFFH) THEN Put1(op, a, b, im)
    if (im >= -0x10000) and (im <= 0xFFFF):
        Put1(op, a, b, im)
    # # ELSE Put1(Mov+U, RH, 0, im DIV 10000H);
    else:
        Put1(Mov+U, RH, 0, im/0x10000)
        # # IF im MOD 10000H # 0 THEN Put1(Ior, RH, RH, im MOD 10000H) END ;
        if (im & 0xFFFF) != 0:
            Put1(Ior, RH, RH, im & 0xFFFF)
        # # Put0(op, a, b, RH)
        Put0(op, a, b, RH)
    # # END
# # END Put1a;

# # PROCEDURE Put2(op, a, b, off: LONGINT);
def Put2(op, a, b, off):
    # # BEGIN (*emit load/store instruction*)
    # emit load/store instruction
    # # code[pc] := ((op * 10H + a) * 10H + b) * 100000H + (off MOD 100000H); INC(pc)
    code.append(((op * 0x10 + a) * 0x10 + b) * 0x100000 + (off & 0xFFFFF))
# # END Put2;

# # PROCEDURE Put3(op, cond, off: LONGINT);
def Put3(op, cond, off):
    # # BEGIN (*emit branch instruction*)
    # emit branch instruction
    # # code[pc] := ((op+12) * 10H + cond) * 1000000H + (off MOD 1000000H); INC(pc)
    code.append(((op+12) * 0x10 + cond) * 0x1000000 + (off & 0xFFFFFF))
# # END Put3;

# # PROCEDURE incR;
def incR():
    global RH
    # # BEGIN
    # # IF RH < MT-1 THEN INC(RH) ELSE ORS.Mark("register stack overflow") END
    if RH < MT-1:
        RH += 1
    else:
        print "register stack overflow"
# # END incR;

# # PROCEDURE CheckRegs*;
def CheckRegs():
    global RH
    # # BEGIN
    # # IF RH # 0 THEN ORS.Mark("Reg Stack"); RH := 0 END ;
    if RH != 0:
        print "Reg Stack"
        RH = 0
    # # IF pc >= maxCode - 40 THEN ORS.Mark("Program too long") END
    if len(code) >= maxCode - 40:  # FIXME: should it be made obsolete?
        print "Program too long"
# # END CheckRegs;

# # PROCEDURE SetCC(VAR x: Item; n: LONGINT);
def SetCC(x, n):
    # # BEGIN x.mode := Cond; x.a := 0; x.b := 0; x.r := n
    x.mode = Cond
    x.a = 0
    x.b = 0
    x.r = n
# # END SetCC;

# # PROCEDURE Trap(cond, num: LONGINT);
def Trap(cond, num):
    # # BEGIN num := ORS.Pos()*100H + num*10H + MT; Put3(BLR, cond, num)
    num = num*0x10 + MT  # FIXME: I elect not to use .Pos for now, due to technicalities
    Put3(BLR, cond, num)
# # END Trap;

# handling of forward reference, fixups of branch addresses and constant tables

# # PROCEDURE negated(cond: LONGINT): LONGINT;
def negated(cond):
    # # BEGIN
    # # IF cond < 8 THEN cond := cond+8 ELSE cond := cond-8 END ;
    if cond < 8:
        cond += 8
    else:
        cond -= 8
    # # RETURN cond
    return cond
# # END negated;

# # PROCEDURE invalSB;
def invalSB():
    global curSB
    # # BEGIN curSB := 1
    curSB = 1
# # END invalSB;

# # PROCEDURE fix(at, with: LONGINT);
def fix(at, _with):
    # # BEGIN code[at] := code[at] DIV C24 * C24 + (with MOD C24)
    code[at] = code[at] # FIXME
# # END fix;

# # PROCEDURE FixLink*(L: LONGINT);
def FixLink(L):
    # # VAR L1: LONGINT;
    # # BEGIN invalSB;
    invalSB()
    # # WHILE L # 0 DO L1 := code[L] MOD 40000H; fix(L, pc-L-1); L := L1 END
    while L != 0:
        L1 = code[L] & 0x3FFFF
        fix(L, len(code)-L-1)
        L = L1
# # END FixLink;

# # PROCEDURE FixLinkWith(L0, dst: LONGINT);
def FixLinkWith(L0, dst):
    # # VAR L1: LONGINT;
    # # BEGIN
    # # WHILE L0 # 0 DO
    while L0 != 0:
        # # L1 := code[L0] MOD C24;
        L1 = code[L0] & 0x3FFFF # FIXME
        # # code[L0] := code[L0] DIV C24 * C24 + ((dst - L0 - 1) MOD C24); L0 := L1
        code[L0] = code[L0] # FIXME
        L0 = L1
    # # END
# # END FixLinkWith;

# # PROCEDURE merged(L0, L1: LONGINT): LONGINT;
def merged(L0, L1):
    # # VAR L2, L3: LONGINT;
    # # BEGIN 
    # # IF L0 # 0 THEN L3 := L0;
    if L0 != 0:
        L3 = L0
        # # REPEAT L2 := L3; L3 := code[L2] MOD 40000H UNTIL L3 = 0;
        while True:
            L2 = L3
            L3 = code[L2] & 0x3FFFF
            if L3 == 0:
                break
        # # code[L2] := code[L2] + L1; L1 := L0
        code[L2] += L1
        L1 = L0
    # # END ;
    # # RETURN L1
    return L1
# # END merged;

# loading of operands and addresses into registers

# # PROCEDURE GetSB(base: LONGINT);
def GetSB(base):
    global fixorgD, curSB
    # # BEGIN
    # # IF (version # 0) & ((base # curSB) OR (base # 0)) THEN
    if (version != 0) and ((base != curSB) or (base != 0)):
        # # Put2(Ldr, SB, -base, pc-fixorgD); fixorgD := pc-1; curSB := base
        Put2(Ldr, SB, -base, len(code)-fixorgD)
        fixorgD = len(code)-1
        curSB = base
    # # END
# # END GetSB;

# # PROCEDURE NilCheck;
def NilCheck():
    # # BEGIN IF check THEN Trap(EQ, 4) END
    if check:
        Trap(EQ, 4)
# # END NilCheck;

# # PROCEDURE load(VAR x: Item);
def load(x):
    # # VAR op: LONGINT;
    # # BEGIN
    # # IF x.type.size = 1 THEN op := Ldr+1 ELSE op := Ldr END ;
    if x.type.size == 1:
        op = Ldr+1
    else:
        op = Ldr
    # # IF x.mode # Reg THEN
    if x.mode == Reg:
        return
    # # IF x.mode = ORB.Const THEN
    if x.mode == symbols.Const:
        # # IF x.type.form = ORB.Proc THEN
        if x.type.form == symbols.Proc:
            # # IF x.r > 0 THEN ORS.Mark("not allowed")
            if x.r > 0:
                print "not allowed"
            # # ELSIF x.r = 0 THEN Put3(BL, 7, 0); Put1a(Sub, RH, LNK, pc*4 - x.a)
            if x.r == 0:
                Put3(BL, 7, 0)
                Put1a(Sub, RH, LNK, len(code)*4 - x.a)
            # # ELSE GetSB(x.r); Put1(Add, RH, SB, x.a + 100H) (*mark as progbase-relative*)
            else:
                GetSB(x.r)
                Put1(Add, RH, SB, x.a + 0x100) # mark as progbase-relative
            # # END
        # # ELSIF (x.a <= 0FFFFH) & (x.a >= -10000H) THEN Put1(Mov, RH, 0, x.a)
        elif (x.a <= 0xFFFF) and (x.a >= -0x10000):
            Put1(Mov, RH, 0, x.a)
        # # ELSE Put1(Mov+U, RH, 0, x.a DIV 10000H MOD 10000H);
        else:
            Put1(Mov+U, RH, 0, x.a DIV 0x10000 MOD 0x10000)
            # # IF x.a MOD 10000H # 0 THEN Put1(Ior, RH, RH, x.a MOD 10000H) END
            if (x.a & 0xFFFF) != 0:
                Put1(Ior, RH, RH, x.a & 0xFFFF)
        # # END ;
        # # x.r := RH; incR
        x.r = RH
        incR()
    # # ELSIF x.mode = ORB.Var THEN
    elif x.mode == symbols.Var:
        # # IF x.r > 0 THEN (*local*) Put2(op, RH, SP, x.a + frame)
        if x.r > 0:  # local
            Put2(op, RH, SP, x.a + frame)
        # # ELSE GetSB(x.r); Put2(op, RH, SB, x.a)
        else:
            GetSB(x.r)
            Put2(op, RH, SB, x.a)
        # # END ;
        # # x.r := RH; incR
        x.r = RH
        incR()
    # # ELSIF x.mode = ORB.Par THEN Put2(Ldr, RH, SP, x.a + frame); Put2(op, RH, RH, x.b); x.r := RH; incR
    elif x.mode == symbols.Par:
        Put2(Ldr, RH, SP, x.a + frame)
        Put2(op, RH, RH, x.b)
        x.r = RH
        incR()
    # # ELSIF x.mode = RegI THEN Put2(op, x.r, x.r, x.a)
    elif x.mode == RegI:
        Put2(op, x.r, x.r, x.a)
    # # ELSIF x.mode = Cond THEN
    elif x.mode == Cond:
        # # Put3(BC, negated(x.r), 2);
        Put3(BC, negated(x.r), 2)
        # # FixLink(x.b); Put1(Mov, RH, 0, 1); Put3(BC, 7, 1);
        FixLink(x.b)
        Put1(Mov, RH, 0, 1)
        Put3(BC, 7, 1)
        # # FixLink(x.a); Put1(Mov, RH, 0, 0); x.r := RH; incR
        FixLink(x.a)
        Put1(Mov, RH, 0, 0)
        x.r = RH
        incR()
    # # END ;
    # # x.mode := Reg
    x.mode = Reg
    # # END
# # END load;

# # PROCEDURE loadAdr(VAR x: Item);
def loadAdr(x):
    # # BEGIN
    # # IF x.mode = ORB.Var THEN
    if x.mode == symbols.Var:
        # # IF x.r > 0 THEN (*local*) Put1a(Add, RH, SP, x.a + frame)
        if x.r > 0:  # local
            Put1a(Add, RH, SP, x.a + frame)
        # # ELSE GetSB(x.r); Put1a(Add, RH, SB, x.a)
        else:
            GetSB(x.r)
            Put1a(Add, RH, SB, x.a)
        # # END ;
        # # x.r := RH; incR
        x.r = RH
        incR()
    # # ELSIF x.mode = ORB.Par THEN Put2(Ldr, RH, SP, x.a + frame);
    elif x.mode == symbols.Par:
        # # IF x.b # 0 THEN Put1a(Add, RH, RH, x.b) END ;
        if x.b != 0:
            Put1a(Add, RH, RH, x.b)
        # # x.r := RH; incR
        x.r = RH
        incR()
    # # ELSIF x.mode = RegI THEN
    elif x.mode == RegI:
        # # IF x.a # 0 THEN Put1a(Add, x.r, x.r, x.a) END
        if x.a != 0:
            Put1a(Add, x.r, x.r, x.a)
    # # ELSE ORS.Mark("address error")
    else:
        print "address error"
    # # END ;
    # # x.mode := Reg
    x.mode = Reg
# # END loadAdr;

# # PROCEDURE loadCond(VAR x: Item);
def loadCond(x):
    global RH
    # # BEGIN
    # # IF x.type.form = ORB.Bool THEN
    if x.type.form == symbols.Bool:
        # # IF x.mode = ORB.Const THEN x.r := 15 - x.a*8
        if x.mode == symbols.Const:
            x.r = 15 - x.a*8
        # # ELSE load(x);
        else:
            load(x)
            # # IF code[pc-1] DIV 40000000H # -2 THEN Put1(Cmp, x.r, x.r, 0) END ;
            if code[-1] / 0x40000000 != -2:
                Put1(Cmp, x.r, x.r, 0)
            # # x.r := NE; DEC(RH)
            x.r = NE
            RH -= 1
        # # END ;
        # # x.mode := Cond; x.a := 0; x.b := 0
        x.mode = Cond
        x.a = 0
        x.b = 0
    # # ELSE ORS.Mark("not Boolean?")
    else:
        print "not Boolean?"
    # # END
# # END loadCond;

# # PROCEDURE loadTypTagAdr(T: ORB.Type);
def loadTypTagAdr(T):
    # # VAR x: Item;
    # # BEGIN x.mode := ORB.Var; x.a := T.len; x.r := -T.mno; loadAdr(x)
    x.mode = symbols.Var
    x.a = T._len
    x.r = -T.mno
    loadAdr(x)
# # END loadTypTagAdr;

# # PROCEDURE loadStringAdr(VAR x: Item);
def loadStringAdr(x):
    # # BEGIN GetSB(0); Put1a(Add, RH, SB, varsize+x.a); x.mode := Reg; x.r := RH; incR
    GetSB(0)
    Put1a(Add, RH, SB, varsize+x.a)
    x.mode = Reg
    x.r = RH
    incR()
# # END loadStringAdr;

# Items: Conversion from constants or from Objects on the Heap to Items on the Stack

# # PROCEDURE MakeConstItem*(VAR x: Item; typ: ORB.Type; val: LONGINT);
def MakeConstItem(x, typ, val):
    # # BEGIN x.mode := ORB.Const; x.type := typ; x.a := val
    x.mode = symbols.Const
    x.type = typ
    x.a = val
# # END MakeConstItem;

# # PROCEDURE MakeRealItem*(VAR x: Item; val: REAL);
def MakeRealItem(x, val):
    # # BEGIN x.mode := ORB.Const; x.type := ORB.realType; x.a := SYSTEM.VAL(LONGINT, val)
    x.mode = symbols.Const
    x.type = symbols.realType
    x.a = SYSTEM.VAL(LONGINT, val)  # FIXME
# # END MakeRealItem;

# FIXME
# # PROCEDURE MakeStringItem*(VAR x: Item; len: LONGINT); (*copies string from ORS-buffer to ORG-string array*)
def MakeStringItem(x, slen): #copies string from ORS-buffer to ORG-string array
    # # VAR i: LONGINT;
    # # BEGIN x.mode := ORB.Const; x.type := ORB.strType; x.a := strx; x.b := len; i := 0;
    x.mode = symbols.Const
    x.type = symbols.strType
    x.a = strx
    x.b = slen
    i = 0
    # # IF strx + len + 4 < maxStrx THEN
    if strx + slen + 4 < maxStrx:
        # # WHILE len > 0 DO str[strx] := ORS.str[i]; INC(strx); INC(i); DEC(len) END ;
        while slen > 0:
            _str[strx] = scanner._str[i]
            strx += 1
            i += 1
            slen -= 1
        # # WHILE strx MOD 4 # 0 DO str[strx] := 0X; INC(strx) END
        while strx % 4 != 0:
            _str[strx] = '\0'
            strx += 1
    # # ELSE ORS.Mark("too many strings")
    else:
        print "too many strings"
    # # END
# # END MakeStringItem;

# # PROCEDURE MakeItem*(VAR x: Item; y: ORB.Object; curlev: LONGINT);
def MakeItem(x, y, curlev):
    # # BEGIN x.mode := y.class; x.type := y.type; x.a := y.val; x.rdo := y.rdo;
    x.mode = y.klass
    x.type = y.type
    x.a = y.val
    x.rdo = y.rdo
    # # IF y.class = ORB.Par THEN x.b := 0
    if y.klass == symbols.Par:
        x.b = 0
    # # ELSIF y.class = ORB.Typ THEN x.a := y.type.len; x.r := -y.lev
    elif y.klass == symbols.Typ:
        x.a = y.type._len
        x.r := -y.lev
    # # ELSIF (y.class = ORB.Const) & (y.type.form = ORB.String) THEN x.b := y.lev  (*len*)
    elif (y.klass == symbols.Const) and (y.type.form == symbols.String):
        x.b = y.lev  # len
    # # ELSE x.r := y.lev
    else:
        x.r = y.lev
    # # END ;
    # # IF (y.lev > 0) & (y.lev # curlev) & (y.class # ORB.Const) THEN ORS.Mark("level error, not accessible") END
    if (y.lev > 0) and (y.lev != curlev) and (y.klass != symbols.Const):
        print "level error, not accessible"
# # END MakeItem;

# Code generation for Selectors, Variables, Constants

# # PROCEDURE Field*(VAR x: Item; y: ORB.Object);   (* x := x.y *)
def Field(x, y):  # x := x.y
    # # BEGIN;
    # # IF x.mode = ORB.Var THEN
    if x.mode == symbols.Var:
        # # IF x.r >= 0 THEN x.a := x.a + y.val
        if x.r >= 0:
            x.a += y.val
        # # ELSE loadAdr(x); x.mode := RegI; x.a := y.val
        else:
            loadAdr(x)
            x.mode = RegI
            x.a = y.val
        # # END
    # # ELSIF x.mode = RegI THEN x.a := x.a + y.val
    elif x.mode == RegI:
        x.a += y.val
    # # ELSIF x.mode = ORB.Par THEN x.b := x.b + y.val
    elif x.mode == symbols.Par:
        x.b += y.val
    # # END
# # END Field;

# # PROCEDURE Index*(VAR x, y: Item);   (* x := x[y] *)
def Index(x, y):  # x := x[y]
    global RH
    # # VAR s, lim: LONGINT;
    # # BEGIN s := x.type.base.size; lim := x.type.len;
    s = x.type.base.size
    lim = x.type._len
    # # IF (y.mode = ORB.Const) & (lim >= 0) THEN
    if (y.mode = symbols.Const) and (lim >= 0):
        # # IF (y.a < 0) OR (y.a >= lim) THEN ORS.Mark("bad index") END ;
        if (y.a < 0) OR (y.a >= lim):
            print "bad index"
        # # IF x.mode IN {ORB.Var, RegI} THEN x.a := y.a * s + x.a
        if x.mode in (symbols.Var, RegI):
            x.a += y.a * s
        # # ELSIF x.mode = ORB.Par THEN x.b := y.a * s + x.b
        elif x.mode == symbols.Par:
            x.b += y.a * s
        # # END
    # # ELSE load(y);
    else:
        load(y)
        # # IF check THEN  (*check array bounds*)
        if check():  # check array bounds
            # # IF lim >= 0 THEN Put1a(Cmp, RH, y.r, lim)
            if lim >= 0:
                Put1a(Cmp, RH, y.r, lim)
            # # ELSE (*open array*)
            else:  # open array
            # # IF x.mode IN {ORB.Var, ORB.Par} THEN Put2(Ldr, RH, SP, x.a+4+frame); Put0(Cmp, RH, y.r, RH)
                if x.mode in (symbols.Var, symbols.Par):
                    Put2(Ldr, RH, SP, x.a+4+frame)
                    Put0(Cmp, RH, y.r, RH)
                # # ELSE ORS.Mark("error in Index")
                else:
                    print "error in Index"
                # # END
            # # END ;
            # # Trap(10, 1)  (*BCC*)
            Trap(10, 1)  # BCC
        # # END ;
        # # IF s = 4 THEN Put1(Lsl, y.r, y.r, 2) ELSIF s > 1 THEN Put1a(Mul, y.r, y.r, s) END ;
        if s == 4:
            Put1(Lsl, y.r, y.r, 2)
        elif s > 1:
            Put1a(Mul, y.r, y.r, s)
        # # IF x.mode = ORB.Var THEN
        if x.mode == symbols.Var:
            # # IF x.r > 0 THEN Put0(Add, y.r, SP, y.r); INC(x.a, frame)
            if x.r > 0:
                Put0(Add, y.r, SP, y.r)
                x.a += frame
            # # ELSE GetSB(x.r);
            else:
                GetSB(x.r)
                # # IF x.r = 0 THEN Put0(Add, y.r, SB, y.r)
                if x.r == 0:
                    Put0(Add, y.r, SB, y.r)
                # # ELSE Put1a(Add, RH, SB, x.a); Put0(Add, y.r, RH, y.r); x.a := 0
                else:
                    Put1a(Add, RH, SB, x.a)
                    Put0(Add, y.r, RH, y.r)
                    x.a = 0
                # # END
            # # END ;
            # # x.r := y.r; x.mode := RegI
            x.r = y.r
            x.mode = RegI
        # # ELSIF x.mode = ORB.Par THEN
        elif x.mode == symbols.Par:
            # # Put2(Ldr, RH, SP, x.a + frame);
            Put2(Ldr, RH, SP, x.a + frame)
            # # Put0(Add, y.r, RH, y.r); x.mode := RegI; x.r := y.r; x.a := x.b
            Put0(Add, y.r, RH, y.r)
            x.mode = RegI
            x.r = y.r
            x.a = x.b
        # # ELSIF x.mode = RegI THEN Put0(Add, x.r, x.r, y.r); DEC(RH)
        elif x.mode == RegI:
            Put0(Add, x.r, x.r, y.r)
            RH -= 1
        # # END
    # # END
# # END Index;

# # PROCEDURE DeRef*(VAR x: Item);
def DeRef(x):
    # # BEGIN
    # # IF x.mode = ORB.Var THEN
    if x.mode == symbols.Var:
        # # IF x.r > 0 THEN (*local*) Put2(Ldr, RH, SP, x.a + frame) ELSE GetSB(x.r); Put2(Ldr, RH, SB, x.a) END ;
        if x.r > 0: # local
            Put2(Ldr, RH, SP, x.a + frame)
        else:
            GetSB(x.r)
            Put2(Ldr, RH, SB, x.a)
        # # NilCheck; x.r := RH; incR
        NilCheck()
        x.r = RH
        incR()
    # # ELSIF x.mode = ORB.Par THEN
    elif x.mode == symbols.Par:
        # # Put2(Ldr, RH, SP, x.a + frame); Put2(Ldr, RH, RH, x.b); NilCheck; x.r := RH; incR
        Put2(Ldr, RH, SP, x.a + frame)
        Put2(Ldr, RH, RH, x.b)
        NilCheck()
        x.r = RH
        incR()
    # # ELSIF x.mode = RegI THEN Put2(Ldr, x.r, x.r, x.a); NilCheck
    elif x.mode == RegI:
        Put2(Ldr, x.r, x.r, x.a)
        NilCheck()
    # # ELSIF x.mode # Reg THEN ORS.Mark("bad mode in DeRef")
    elif x.mode != Reg:
        print "bad mode in DeRef"
    # # END ;
    # # x.mode := RegI; x.a := 0; x.b := 0
    x.mode = RegI
    x.a = 0
    x.b = 0
# # END DeRef;

# # PROCEDURE Q(T: ORB.Type; VAR dcw: LONGINT);
def Q(T, dcw):
    # # BEGIN (*one entry of type descriptor extension table*)
    # one entry of type descriptor extension table
    # # IF T.base # NIL THEN
    if T.base != None:
        # # Q(T.base, dcw); data[dcw] := (T.mno*1000H + T.len) * 1000H + dcw - fixorgT;
        Q(T.base, dcw)
        data[dcw] = (T.mno*0x1000 + T._len) * 0x1000 + dcw - fixorgT
        # # fixorgT := dcw; INC(dcw)
        fixorgT = dcw
        dcw += 1
    # # END
# # END Q;

# # PROCEDURE FindPtrFlds(typ: ORB.Type; off: LONGINT; VAR dcw: LONGINT);
def FindPtrFlds(typ, off, dcw):
    # # VAR fld: ORB.Object; i, s: LONGINT;
    # # BEGIN
    # # IF (typ.form = ORB.Pointer) OR (typ.form = ORB.NilTyp) THEN data[dcw] := off; INC(dcw)
    if (typ.form == symbols.Pointer) or (typ.form == symbols.NilTyp):
        data[dcw] = off
        dcw += 1
    # # ELSIF typ.form = ORB.Record THEN
    elif typ.form == symbols.Record:
        # # fld := typ.dsc;
        fld = typ.dsc
        # # WHILE fld # NIL DO FindPtrFlds(fld.type, fld.val + off, dcw); fld := fld.next END
        while fld != None:
            FindPtrFlds(fld.type, fld.val + off, dcw)
            fld := fld.next
    # # ELSIF typ.form = ORB.Array THEN
    elif typ.form == symbols.Array:
        # # s := typ.base.size;
        s = typ.base.size
        # # FOR i := 0 TO typ.len-1 DO FindPtrFlds(typ.base, i*s + off, dcw) END
        for i in range(typ._len):
            FindPtrFlds(typ.base, i*s + off, dcw)
    # # END
# # END FindPtrFlds;

# # PROCEDURE BuildTD*(T: ORB.Type; VAR dc: LONGINT);
def BuildTD(T, dc):
    # # VAR dcw, k, s: LONGINT;  (*dcw = word address*)
    # # BEGIN dcw := dc DIV 4; s := T.size; (*convert size for heap allocation*)
    dcw = dc / 4  # dcw = word address
    s = T.size  # convert size for heap allocation
    # # IF s <= 24 THEN s := 32 ELSIF s <= 56 THEN s := 64 ELSIF s <= 120 THEN s := 128
    if s <= 24:
        s = 32
    elif s <= 56:
        s = 64
    elif s <= 120:
        s := 128
    # # ELSE s := (s+263) DIV 256 * 256
    else:
        s = (s+263) / 256 * 256
    # # END ;
    # # T.len := dc; data[dcw] := s; INC(dcw);
    T.len = dc
    data[dcw] = s
    dcw += 1
    # # k := T.nofpar;   (*extension level!*)
    k = T.nofpar   # extension level!
    # # IF k > 3 THEN ORS.Mark("ext level too large")
    if k > 3:
        print "ext level too large"
    # # ELSE Q(T, dcw);
    else:
        Q(T, dcw)
        # # WHILE k < 3 DO data[dcw] := -1; INC(dcw); INC(k) END
        while k < 3:
            data[dcw] = -1
            dcw += 1
            k += 1
    # # END ;
    # # FindPtrFlds(T, 0, dcw); data[dcw] := -1; INC(dcw); tdx := dcw; dc := dcw*4;
    FindPtrFlds(T, 0, dcw)
    data[dcw] = -1
    dcw += 1
    tdx = dcw
    dc = dcw*4
    # # IF tdx >= maxTD THEN ORS.Mark("too many record types"); tdx := 0 END
    if tdx >= maxTD:
        print "too many record types")
        tdx = 0
# # END BuildTD;

# # PROCEDURE TypeTest*(VAR x: Item; T: ORB.Type; varpar, isguard: BOOLEAN);
def TypeTest(x, T, varpar, isguard):
    global RH
    # # VAR pc0: LONGINT;
    # # BEGIN (*fetch tag into RH*)
    # fetch tag into RH
    # # IF varpar THEN Put2(Ldr, RH, SP, x.a+4+frame)
    if varpar:
        Put2(Ldr, RH, SP, x.a+4+frame)
    # # ELSE load(x);
    else:
        load(x)
        # # pc0 := pc; Put3(BC, EQ, 0);  (*NIL belongs to every pointer type*)
        pc0 = len(code)
        Put3(BC, EQ, 0)  # NIL belongs to every pointer type
        # # Put2(Ldr, RH, x.r, -8)
        Put2(Ldr, RH, x.r, -8)
    # # END ;
    # # Put2(Ldr, RH, RH, T.nofpar*4); incR;
    Put2(Ldr, RH, RH, T.nofpar*4)
    incR()
    # # loadTypTagAdr(T);  (*tag of T*)
    loadTypTagAdr(T)  # tag of T
    # # Put0(Cmp, RH-1, RH-1, RH-2); DEC(RH, 2);
    Put0(Cmp, RH-1, RH-1, RH-2)
    RH -= 2
    # # IF ~varpar THEN fix(pc0, pc - pc0 - 1) END ;
    if !varpar:
        fix(pc0, len(code) - pc0 - 1)
    # # IF isguard THEN
    if isguard:
        # # IF check THEN Trap(NE, 2) END
        if check:
            Trap(NE, 2)
    # # ELSE SetCC(x, EQ);
    else:
        SetCC(x, EQ)
        # # IF ~varpar THEN DEC(RH) END
        if !varpar:
            RH -= 1
    # # END
# # END TypeTest;

# Code generation for Boolean operators

# # PROCEDURE Not*(VAR x: Item);   (* x := ~x *)
def Not(x):  # x := ~x
    # # VAR t: LONGINT;
    # # BEGIN
    # # IF x.mode # Cond THEN loadCond(x) END ;
    if x.mode != Cond:
        loadCond(x)
    # # x.r := negated(x.r); t := x.a; x.a := x.b; x.b := t
    x.r = negated(x.r)
    t = x.a
    x.a = x.b
    x.b := t
# # END Not;

# # PROCEDURE And1*(VAR x: Item);   (* x := x & *)
def And1(x):  # x := x &
    # # BEGIN
    # # IF x.mode # Cond THEN loadCond(x) END ;
    if x.mode != Cond:
        loadCond(x)
    # # Put3(BC, negated(x.r), x.a); x.a := pc-1; FixLink(x.b); x.b := 0
    Put3(BC, negated(x.r), x.a)
    x.a = len(code)-1
    FixLink(x.b)
    x.b = 0
# # END And1;

# # PROCEDURE And2*(VAR x, y: Item);
def And2(x, y):
    # # BEGIN
    # # IF y.mode # Cond THEN loadCond(y) END ;
    if y.mode != Cond:
        loadCond(y)
    # # x.a := merged(y.a, x.a); x.b := y.b; x.r := y.r
    x.a = merged(y.a, x.a)
    x.b = y.b
    x.r = y.r
# # END And2;

# # PROCEDURE Or1*(VAR x: Item);   (* x := x OR *)
def Or1(x):  # x := x OR
    # # BEGIN
    # # IF x.mode # Cond THEN loadCond(x) END ;
    if x.mode != Cond:
        loadCond(x)
    # # Put3(BC, x.r, x.b);  x.b := pc-1; FixLink(x.a); x.a := 0
    Put3(BC, x.r, x.b)
    x.b = len(code)-1
    FixLink(x.a)
    x.a = 0
# # END Or1;

# # PROCEDURE Or2*(VAR x, y: Item);
def Or2(x, y):
    # # BEGIN
    # # IF y.mode # Cond THEN loadCond(y) END ;
    if y.mode != Cond:
        loadCond(y)
    # # x.a := y.a; x.b := merged(y.b, x.b); x.r := y.r
    x.a = y.a
    x.b = merged(y.b, x.b)
    x.r := y.r
# # END Or2;

# Code generation for arithmetic operators

# # PROCEDURE Neg*(VAR x: Item);   (* x := -x *)
def Neg(x):  # x := -x
    # # BEGIN
    # # IF x.type.form = ORB.Int THEN
    if x.type.form == symbols.Int:
        # # IF x.mode = ORB.Const THEN x.a := -x.a
        if x.mode == symbols.Const:
            x.a := -x.a
        # # ELSE load(x); Put1(Mov, RH, 0, 0); Put0(Sub, x.r, RH, x.r)
        else:
            load(x)
            Put1(Mov, RH, 0, 0)
            Put0(Sub, x.r, RH, x.r)
        # # END
    # # ELSIF x.type.form = ORB.Real THEN
    elif x.type.form == symbols.Real:
        # # IF x.mode = ORB.Const THEN x.a := x.a + 7FFFFFFFH + 1
        if x.mode == symbols.Const:
            x.a += 0x7FFFFFFF + 1  # FIXME
        # # ELSE load(x); Put1(Mov, RH, 0, 0); Put0(Fsb, x.r, RH, x.r)
        else:
            load(x)
            Put1(Mov, RH, 0, 0)
            Put0(Fsb, x.r, RH, x.r)
        # # END
    # # ELSE (*form = Set*)
    else:  # form = Set
        # # IF x.mode = ORB.Const THEN x.a := -x.a-1 
        if x.mode == symbols.Const:
            x.a = -x.a-1 
        # # ELSE load(x); Put1(Xor, x.r, x.r, -1)
        else:
            load(x)
            Put1(Xor, x.r, x.r, -1)
        # # END
    # # END
# # END Neg;

# # PROCEDURE AddOp*(op: LONGINT; VAR x, y: Item);   (* x := x +- y *)
def AddOp(op, x, y):  # x := x +- y
    global RH
    # # BEGIN
    # # IF op = ORS.plus THEN
    if op == ORS.plus:
        # # IF (x.mode = ORB.Const) & (y.mode = ORB.Const) THEN x.a := x.a + y.a
        if (x.mode == symbols.Const) and (y.mode == symbols.Const):
            x.a += y.a
        # # ELSIF y.mode = ORB.Const THEN load(x);
        elif y.mode == symbols.Const:
            load(x)
            # # IF y.a # 0 THEN Put1a(Add, x.r, x.r, y.a) END
            if y.a != 0:
                Put1a(Add, x.r, x.r, y.a)
        # # ELSE load(x); load(y); Put0(Add, RH-2, x.r, y.r); DEC(RH); x.r := RH-1
        else:
            load(x)
            load(y)
            Put0(Add, RH-2, x.r, y.r)
            RH -= 1
            x.r = RH-1
        # # END
    # # ELSE (*op = ORS.minus*)
    else:  # op = ORS.minus
        # # IF (x.mode = ORB.Const) & (y.mode = ORB.Const) THEN x.a := x.a - y.a
        if (x.mode == symbols.Const) and (y.mode == symbols.Const):
            x.a -= y.a
        # # ELSIF y.mode = ORB.Const THEN load(x);
            load(x)
            # # IF y.a # 0 THEN Put1a(Sub, x.r, x.r, y.a) END
            if y.a != 0:
                Put1a(Sub, x.r, x.r, y.a)
        # # ELSE load(x); load(y); Put0(Sub, RH-2, x.r, y.r); DEC(RH); x.r := RH-1
        else:
            load(x)
            load(y)
            Put0(Sub, RH-2, x.r, y.r)
            RH -= 1
            x.r = RH-1
        # # END
    # # END
# # END AddOp;

# # PROCEDURE log2(m: LONGINT; VAR e: LONGINT): LONGINT;
def log2(m, e):  # FIXME
    # # BEGIN e := 0;
    e = 0
    # # WHILE ~ODD(m) DO m := m DIV 2; INC(e) END ;
    while (m % 2) == 0:
        m /= 2
    e += 1
    # # RETURN m
    return m
# # END log2;
  
# # PROCEDURE MulOp*(VAR x, y: Item);   (* x := x * y *)
def MulOp(x, y):  # x := x * y
    global RH
    # # VAR e: LONGINT;
    # # BEGIN
    # # IF (x.mode = ORB.Const) & (y.mode = ORB.Const) THEN x.a := x.a * y.a
    if (x.mode == symbols.Const) and (y.mode == symbols.Const):
        x.a *= x.a
    # # ELSIF (y.mode = ORB.Const) & (y.a >= 2) & (log2(y.a, e) = 1) THEN load(x); Put1(Lsl, x.r, x.r, e)
    elif (y.mode == symbols.Const) and (y.a >= 2) and (log2(y.a, e) == 1):  # FIXME
        load(x)
        Put1(Lsl, x.r, x.r, e)
    # # ELSIF y.mode = ORB.Const THEN load(x); Put1a(Mul, x.r, x.r, y.a)
    elif y.mode == symbols.Const:
        load(x)
        Put1a(Mul, x.r, x.r, y.a)
    # # ELSIF (x.mode = ORB.Const) & (x.a >= 2) & (log2(x.a, e) = 1) THEN load(y); Put1(Lsl, y.r, y.r, e); x.mode := Reg; x.r := y.r
    elif (x.mode == symbols.Const) and (x.a >= 2) and (log2(x.a, e) == 1):  # FIXME
        load(y)
        Put1(Lsl, y.r, y.r, e)
        x.mode = Reg
        x.r = y.r
    # # ELSIF x.mode = ORB.Const THEN load(y); Put1a(Mul, y.r, y.r, x.a); x.mode := Reg; x.r := y.r
    elif x.mode == symbols.Const:
        load(y)
        Put1a(Mul, y.r, y.r, x.a)
        x.mode = Reg
        x.r = y.r
    # # ELSE load(x); load(y); Put0(Mul, RH-2, x.r, y.r); DEC(RH); x.r := RH-1
    else:
        load(x)
        load(y)
        Put0(Mul, RH-2, x.r, y.r)
        RH -= 1
        x.r = RH-1
    # # END
# # END MulOp;

# # PROCEDURE DivOp*(op: LONGINT; VAR x, y: Item);   (* x := x op y *)
def DivOp(op, x, y):  # x := x op y
    global RH
    # # VAR e: LONGINT;
    # # BEGIN
    # # IF op = ORS.div THEN
    if op == ORS.div:
        # # IF (x.mode = ORB.Const) & (y.mode = ORB.Const) THEN
        if (x.mode == symbols.Const) and (y.mode == symbols.Const):
            # # IF y.a > 0 THEN x.a := x.a DIV y.a ELSE ORS.Mark("bad divisor") END
            if y.a > 0:
                x.a /= y.a
            else:
                print "bad divisor"
        # # ELSIF (y.mode = ORB.Const) & (y.a >= 2) & (log2(y.a, e) = 1) THEN load(x); Put1(Asr, x.r, x.r, e)
        elif (y.mode == symbols.Const) and (y.a >= 2) and (log2(y.a, e) == 1):  # FIXME
            load(x)
            Put1(Asr, x.r, x.r, e)
        # # ELSIF y.mode = ORB.Const THEN
        elif y.mode == symbols.Const:
            # # IF y.a > 0 THEN load(x); Put1a(Div, x.r, x.r, y.a) ELSE ORS.Mark("bad divisor") END
            if y.a > 0:
                load(x)
                Put1a(Div, x.r, x.r, y.a)
            else:
                print "bad divisor"
        # # ELSE load(y);
        else:
            load(y)
            # # IF check THEN Trap(LE, 6) END ;
            if check:
                Trap(LE, 6)
            # # load(x); Put0(Div, RH-2, x.r, y.r); DEC(RH); x.r := RH-1
            load(x)
            Put0(Div, RH-2, x.r, y.r)
            RH -= 1
            x.r = RH-1
        # # END
    # # ELSE (*op = ORS.mod*)
    else:  # op == ORS.mod
        # # IF (x.mode = ORB.Const) & (y.mode = ORB.Const) THEN
        if (x.mode == symbols.Const) and (y.mode == symbols.Const):
            # # IF y.a > 0 THEN x.a := x.a MOD y.a ELSE ORS.Mark("bad modulus") END
            if y.a > 0:
                x.a %= y.a
            else:
                print "bad modulus"
        # # ELSIF (y.mode = ORB.Const) & (y.a >= 2) & (log2(y.a, e) = 1) THEN load(x);
        elif (y.mode == symbols.Const) and (y.a >= 2) and (log2(y.a, e) == 1):  # FIXME
            load(x)
            # # IF e <= 16 THEN Put1(And, x.r, x.r, y.a-1) ELSE Put1(Lsl, x.r, x.r, 32-e); Put1(Ror, x.r, x.r, 32-e) END
            if e <= 16:
                Put1(And, x.r, x.r, y.a-1)
            else:
                Put1(Lsl, x.r, x.r, 32-e)
                Put1(Ror, x.r, x.r, 32-e)
        # # ELSIF y.mode = ORB.Const THEN
        elif y.mode == symbols.Const:
            # # IF y.a > 0 THEN load(x); Put1a(Div, x.r, x.r, y.a); Put0(Mov+U, x.r, 0, 0) ELSE ORS.Mark("bad modulus") END
            if y.a > 0:
                load(x)
                Put1a(Div, x.r, x.r, y.a)
                Put0(Mov+U, x.r, 0, 0)
            else:
                print "bad modulus"
        # # ELSE load(y);
        else:
            load(y);
            # # IF check THEN Trap(LE, 6) END ;
            if check:
                Trap(LE, 6)
            # # load(x); Put0(Div, RH-2, x.r, y.r); Put0(Mov+U, RH-2, 0, 0); DEC(RH); x.r := RH-1
            load(x)
            Put0(Div, RH-2, x.r, y.r)
            Put0(Mov+U, RH-2, 0, 0)
            RH -= 1
            x.r = RH-1
        # # END
    # # END
# # END DivOp;

# Code generation for REAL operators

# # PROCEDURE RealOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
def RealOp(op, x, y):  # x := x op y
    global RH
    # # BEGIN load(x); load(y);
    load(x)
    load(y)
    # # IF op = ORS.plus THEN Put0(Fad, RH-2, x.r, y.r)
    if op == ORS.plus:
        Put0(Fad, RH-2, x.r, y.r)
    # # ELSIF op = ORS.minus THEN Put0(Fsb, RH-2, x.r, y.r)
    elif op == ORS.minus:
        Put0(Fsb, RH-2, x.r, y.r)
    # # ELSIF op = ORS.times THEN Put0(Fml, RH-2, x.r, y.r)
    elif op == ORS.times:
        Put0(Fml, RH-2, x.r, y.r)
    # # ELSIF op = ORS.rdiv THEN Put0(Fdv, RH-2, x.r, y.r)
    elif op == ORS.rdiv:
        Put0(Fdv, RH-2, x.r, y.r)
    # # END ;
    # # DEC(RH); x.r := RH-1
    RH -= 1
    x.r = RH-1
# # END RealOp;

# Code generation for set operators

# # PROCEDURE Singleton*(VAR x: Item);  (* x := {x} *)
def Singleton(x):  # x := {x}
    # # BEGIN
    # # IF x.mode = ORB.Const THEN x.a := LSL(1, x.a)
    if x.mode == symbols.Const:
        x.a = LSL(1, x.a)
    # # ELSE load(x); Put1(Mov, RH, 0, 1); Put0(Lsl, x.r, RH,  x.r)
    else:
        load(x)
        Put1(Mov, RH, 0, 1)
        Put0(Lsl, x.r, RH,  x.r)
    # # END
# # END Singleton;

# # PROCEDURE Set*(VAR x, y: Item);   (* x := {x .. y} *)
def Set(x, y):  # x := {x .. y}
    global RH
    # # BEGIN
    # # IF (x.mode = ORB.Const) & ( y.mode = ORB.Const) THEN
    if (x.mode == symbols.Const) and (y.mode == symbols.Const):
        # # IF x.a <= y.a THEN x.a := LSL(2, y.a) - LSL(1, x.a) ELSE x.a := 0 END
        if x.a <= y.a:
            x.a = LSL(2, y.a) - LSL(1, x.a)
        else:
            x.a = 0
    # # ELSE
    else:
        # # IF (x.mode = ORB.Const) & (x.a < 16) THEN x.a := LSL(-1, x.a)
        if (x.mode == symbols.Const) and (x.a < 16):
            x.a = LSL(-1, x.a)
        # # ELSE load(x); Put1(Mov, RH, 0, -1); Put0(Lsl, x.r, RH, x.r)
        else:
            load(x)
            Put1(Mov, RH, 0, -1)
            Put0(Lsl, x.r, RH, x.r)
        # # END ;
        # # IF (y.mode = ORB.Const) & (y.a < 16) THEN Put1(Mov, RH, 0, LSL(-2, y.a)); y.mode := Reg; y.r := RH; incR
        if (y.mode == symbols.Const) and (y.a < 16):
            Put1(Mov, RH, 0, LSL(-2, y.a))
            y.mode = Reg
            y.r = RH
            incR()
        # # ELSE load(y); Put1(Mov, RH, 0, -2); Put0(Lsl, y.r, RH, y.r)
        else:
            load(y)
            Put1(Mov, RH, 0, -2)
            Put0(Lsl, y.r, RH, y.r)
        # # END ;
        # # IF x.mode = ORB.Const THEN
        if x.mode == symbols.Const:
            # # IF x.a # 0 THEN Put1(Xor, y.r, y.r, -1); Put1a(And, RH-1, y.r, x.a) END ;
            if x.a != 0:
                Put1(Xor, y.r, y.r, -1)
                Put1a(And, RH-1, y.r, x.a)
            # # x.mode := Reg; x.r := RH-1
            x.mode = Reg
            x.r = RH-1
        # # ELSE DEC(RH); Put0(Ann, RH-1, x.r, y.r)
        else:
            RH -= 1
            Put0(Ann, RH-1, x.r, y.r)
        # # END
    # # END
# # END Set;

# # PROCEDURE In*(VAR x, y: Item);  (* x := x IN y *)
def In(x, y):  # x := x IN y
    global RH
    # # BEGIN load(y);
    load(y)
    # # IF x.mode = ORB.Const THEN Put1(Ror, y.r, y.r, (x.a + 1) MOD 20H); DEC(RH)
    if x.mode == symbols.Const:
        Put1(Ror, y.r, y.r, (x.a + 1) % 0x20)
        RH -= 1
    # # ELSE load(x); Put1(Add, x.r, x.r, 1); Put0(Ror, y.r, y.r, x.r); DEC(RH, 2)
    else:
        load(x)
        Put1(Add, x.r, x.r, 1)
        Put0(Ror, y.r, y.r, x.r)
        RH -= 2
    # # END ;
    # # SetCC(x, MI)
    SetCC(x, MI)
# # END In;

# # PROCEDURE SetOp*(op: LONGINT; VAR x, y: Item);   (* x := x op y *)
def SetOp(op, x, y):  # x := x op y
    global RH
    # # VAR xset, yset: SET; (*x.type.form = Set*)
    # # BEGIN
    # # IF (x.mode = ORB.Const) & (y.mode = ORB.Const) THEN
    if (x.mode == symbols.Const) and (y.mode == symbols.Const):
        # # xset := SYSTEM.VAL(SET, x.a); yset := SYSTEM.VAL(SET, y.a);
        xset = SYSTEM.VAL(SET, x.a)
        yset = SYSTEM.VAL(SET, y.a)
        # # IF op = ORS.plus THEN xset := xset + yset
        if op == ORS.plus:
            xset += yset
        # # ELSIF op = ORS.minus THEN xset := xset - yset
        elif op == ORS.minus:
            xset -= yset
        # # ELSIF op = ORS.times THEN xset := xset * yset
        elif op == ORS.times:
            xset = xset * yset
        # # ELSIF op = ORS.rdiv THEN xset := xset / yset
        elif op == ORS.rdiv:
            xset := xset / yset
        # # END ;
        # # x.a := SYSTEM.VAL(LONGINT, xset)
        x.a = SYSTEM.VAL(LONGINT, xset)
    # # ELSIF y.mode = ORB.Const THEN
    elif y.mode == symbols.Const:
        # # load(x);
        load(x)
        # # IF op = ORS.plus THEN Put1a(Ior, x.r, x.r, y.a)
        if op == ORS.plus:
            Put1a(Ior, x.r, x.r, y.a)
        # # ELSIF op = ORS.minus THEN Put1a(Ann, x.r, x.r, y.a)
        elif op == ORS.minus:
            Put1a(Ann, x.r, x.r, y.a)
        # # ELSIF op = ORS.times THEN Put1a(And, x.r, x.r, y.a)
        elif op == ORS.times:
            Put1a(And, x.r, x.r, y.a)
        # # ELSIF op = ORS.rdiv THEN Put1a(Xor, x.r, x.r, y.a)
        elif op == ORS.rdiv:
            Put1a(Xor, x.r, x.r, y.a)
        # # END ;
    # # ELSE load(x); load(y);
    else:
        load(x)
        load(y)
        # # IF op = ORS.plus THEN Put0(Ior, RH-2, x.r, y.r)
        if op == ORS.plus:
            Put0(Ior, RH-2, x.r, y.r)
        # # ELSIF op = ORS.minus THEN Put0(Ann, RH-2, x.r, y.r)
        elif op == ORS.minus:
            Put0(Ann, RH-2, x.r, y.r)
        # # ELSIF op = ORS.times THEN Put0(And, RH-2, x.r, y.r)
        elif op == ORS.times:
            Put0(And, RH-2, x.r, y.r)
        # # ELSIF op = ORS.rdiv THEN Put0(Xor, RH-2, x.r, y.r)
        elif op == ORS.rdiv:
            Put0(Xor, RH-2, x.r, y.r)
        # # END ;
        # # DEC(RH); x.r := RH-1
        RH -= 1
        x.r = RH-1
    # # END 
# # END SetOp;

# Code generation for relations

# # PROCEDURE IntRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
def IntRelation(op, x, y):  # x := x < y
    global RH
    # # BEGIN
    # # IF (y.mode = ORB.Const) & (y.type.form # ORB.Proc) THEN
    if (y.mode == symbols.Const) and (y.type.form != symbols.Proc):
        # # load(x);
        load(x)
        # # IF (y.a # 0) OR ~(op IN {ORS.eql, ORS.neq}) OR (code[pc-1] DIV 40000000H # -2) THEN Put1a(Cmp, x.r, x.r, y.a) END ;
        if (y.a != 0) or !(op in (ORS.eql, ORS.neq)) or (code[-1] / 0x40000000 != -2):
            Put1a(Cmp, x.r, x.r, y.a)
        # # DEC(RH)
        RH -= 1
    # # ELSE load(x); load(y); Put0(Cmp, x.r, x.r, y.r); DEC(RH, 2)
    else:
        load(x)
        load(y)
        Put0(Cmp, x.r, x.r, y.r)
        RH -= 2
    # # END ;
    # # SetCC(x, relmap[op - ORS.eql])
    SetCC(x, relmap[op - ORS.eql])
# # END IntRelation;

# # PROCEDURE SetRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
def SetRelation(op, x, y):  # x := x < y
    global RH
    # # BEGIN load(x);
    load(x)
    # # IF (op = ORS.eql) OR (op = ORS.neq) THEN
    if (op == ORS.eql) or (op == ORS.neq):
        # # IF y.mode = ORB.Const THEN Put1a(Cmp, x.r, x.r, y.a); DEC(RH)
        if y.mode == symbols.Const:
            Put1a(Cmp, x.r, x.r, y.a)
            RH -= 1
        # # ELSE load(y); Put0(Cmp, x.r, x.r, y.r); DEC(RH, 2)
        else:
            load(y)
            Put0(Cmp, x.r, x.r, y.r)
            RH -= 2
        # # END ;
        # # SetCC(x, relmap[op - ORS.eql])
        SetCC(x, relmap[op - ORS.eql])
    # # ELSE ORS.Mark("illegal relation") 
    else:
        print "illegal relation"
    # # END
# # END SetRelation;

# # PROCEDURE RealRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
def RealRelation(op, x, y):  # x := x < y
    global RH
    # # BEGIN load(x);
    load(x)
    # # IF (y.mode = ORB.Const) & (y.a = 0) THEN DEC(RH)
    if (y.mode == symbols.Const) and (y.a == 0):
        RH -= 1
    # # ELSE load(y); Put0(Fsb, x.r, x.r, y.r); DEC(RH, 2)
    else:
        load(y)
        Put0(Fsb, x.r, x.r, y.r)
        RH -= 2
    # # END ;
    # # SetCC(x, relmap[op - ORS.eql])
    SetCC(x, relmap[op - ORS.eql])
# # END RealRelation;

# # PROCEDURE StringRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
def StringRelation(op, x, y):  # x := x < y
    global RH
    # # (*x, y are char arrays or strings*)
    # x, y are char arrays or strings
    # # BEGIN
    # # IF x.type.form = ORB.String THEN loadStringAdr(x) ELSE loadAdr(x) END ;
    if x.type.form == symbols.String:
        loadStringAdr(x)
    else:
        loadAdr(x)
    # # IF y.type.form = ORB.String THEN loadStringAdr(y) ELSE loadAdr(y) END ;
    if y.type.form == symbols.String:
        loadStringAdr(y)
    else:
        loadAdr(y)
    # # Put2(Ldr+1, RH, x.r, 0); Put1(Add, x.r, x.r, 1);
    Put2(Ldr+1, RH, x.r, 0)
    Put1(Add, x.r, x.r, 1)
    # # Put2(Ldr+1, RH+1, y.r, 0); Put1(Add, y.r, y.r, 1);
    Put2(Ldr+1, RH+1, y.r, 0)
    Put1(Add, y.r, y.r, 1)
    # # Put0(Cmp, RH+2, RH, RH+1); Put3(BC, NE, 2);
    Put0(Cmp, RH+2, RH, RH+1)
    Put3(BC, NE, 2)
    # # Put1(Cmp, RH+2, RH, 0); Put3(BC, NE, -8);
    Put1(Cmp, RH+2, RH, 0)
    Put3(BC, NE, -8)
    # # DEC(RH, 2); SetCC(x, relmap[op - ORS.eql])
    RH -= 2
    SetCC(x, relmap[op - ORS.eql])
# # END StringRelation;

# Code generation of Assignments

# # PROCEDURE StrToChar*(VAR x: Item);
def StrToChar(x):
    global strx
    # # BEGIN x.type := ORB.charType; DEC(strx, 4); x.a := ORD(str[x.a])
    x.type = symbols.charType
    strx -= 4
    x.a = ord(str[x.a])
# # END StrToChar;

# # PROCEDURE Store*(VAR x, y: Item); (* x := y *)
def Store(x, y):  # x := y
    global RH
    # # VAR op: LONGINT;
    # # BEGIN  load(y);
    load(y)
    # # IF x.type.size = 1 THEN op := Str+1 ELSE op := Str END ;
    if x.type.size == 1:
        op = Str+1
    else:
        op = Str
    # # IF x.mode = ORB.Var THEN
    if x.mode == symbols.Var:
        # # IF x.r > 0 THEN (*local*) Put2(op, y.r, SP, x.a + frame)
        if x.r > 0:  # local
            Put2(op, y.r, SP, x.a + frame)
        # # ELSE GetSB(x.r); Put2(op, y.r, SB, x.a)
        else:
            GetSB(x.r)
            Put2(op, y.r, SB, x.a)
        # # END
    # # ELSIF x.mode = ORB.Par THEN Put2(Ldr, RH, SP, x.a + frame); Put2(op, y.r, RH, x.b);
    elif x.mode == symbols.Par:
        Put2(Ldr, RH, SP, x.a + frame)
        Put2(op, y.r, RH, x.b)
    # # ELSIF x.mode = RegI THEN Put2(op, y.r, x.r, x.a); DEC(RH);
    elif x.mode == RegI:
        Put2(op, y.r, x.r, x.a)
        RH -= 1
    # # ELSE ORS.Mark("bad mode in Store")
    else:
        print "bad mode in Store"
    # # END ;
    # # DEC(RH)
    RH -= 1
# # END Store;

# # PROCEDURE StoreStruct*(VAR x, y: Item); (* x := y, frame = 0 *)
def StoreStruct(x, y):  # x := y, frame = 0
    global RH
    # # VAR s, pc0: LONGINT;
    # # BEGIN loadAdr(x); loadAdr(y);
    loadAdr(x)
    loadAdr(y)
    # # IF (x.type.form = ORB.Array) & (x.type.len > 0) THEN
    if (x.type.form == symbols.Array) and (x.type._len > 0):
        # # IF y.type.len >= 0 THEN 
        if y.type._len >= 0:
            # # IF x.type.len >= y.type.len THEN Put1a(Mov, RH, 0, (y.type.size+3) DIV 4)
            if x.type._len >= y.type._len:
                Put1a(Mov, RH, 0, (y.type.size+3) / 4)
            # # ELSE ORS.Mark("source array too long")
            else:
                print "source array too long")
            # # END
        # # ELSE (*y is open array*)
        else:  # y is open array
            # # Put2(Ldr, RH, SP, y.a+4); s := y.type.base.size;  (*element size*)
            Put2(Ldr, RH, SP, y.a+4)
            s = y.type.base.size  # element size
            # # pc0 := pc; Put3(BC, EQ, 0);
            pc0 = len(code)
            Put3(BC, EQ, 0)
            # # IF s = 1 THEN Put1(Add, RH, RH, 3); Put1(Asr, RH, RH, 2)
            if s == 1:
                Put1(Add, RH, RH, 3)
                Put1(Asr, RH, RH, 2)
            # # ELSIF s # 4 THEN Put1a(Mul, RH, RH, s DIV 4)
            elif s != 4:
                Put1a(Mul, RH, RH, s / 4)
            # # END ;
            # # IF check THEN
            if check:
                # # Put1a(Mov, RH+1, 0, (x.type.size+3) DIV 4); Put0(Cmp, RH+1, RH, RH+1); Trap(GT, 3)
                Put1a(Mov, RH+1, 0, (x.type.size+3) / 4)
                Put0(Cmp, RH+1, RH, RH+1)
                Trap(GT, 3)
            # # END ;
            # # fix(pc0, pc + 5 - pc0)
            fix(pc0, len(code) + 5 - pc0)
        # # END
    # # ELSIF x.type.form = ORB.Record THEN Put1a(Mov, RH, 0, x.type.size DIV 4)
    elif x.type.form == symbols.Record:
        Put1a(Mov, RH, 0, x.type.size / 4)
    # # ELSE ORS.Mark("inadmissible assignment")
    else:
        print "inadmissible assignment"
    # # END ;
    # # Put2(Ldr, RH+1, y.r, 0); Put1(Add, y.r, y.r, 4);
    Put2(Ldr, RH+1, y.r, 0)
    Put1(Add, y.r, y.r, 4)
    # # Put2(Str, RH+1, x.r, 0); Put1(Add, x.r, x.r, 4);
    Put2(Str, RH+1, x.r, 0)
    Put1(Add, x.r, x.r, 4)
    # # Put1(Sub, RH, RH, 1); Put3(BC, NE, -6); DEC(RH, 2)
    Put1(Sub, RH, RH, 1)
    Put3(BC, NE, -6)
    RH -= 2
# # END StoreStruct;

# # PROCEDURE CopyString*(VAR x, y: Item);  (*from x to y*)
def CopyString(x, y):  # from x to y
    global RH
    # # VAR len: LONGINT;
    # # BEGIN loadAdr(y); len := y.type.len;
    loadAdr(y)
    slen = y.type._len
    # # IF len >= 0 THEN
    if slen >= 0:
        # # IF x.b > len THEN ORS.Mark("string too long") END
        if x.b > slen:
            print "string too long"
    # # ELSIF check THEN Put2(Ldr, RH, y.r, 4);  (*array length check*)
    elif check:
        Put2(Ldr, RH, y.r, 4)  # array length check
        # # Put1(Cmp, RH, RH, x.b); Trap(NE, 3)
        Put1(Cmp, RH, RH, x.b)
        Trap(NE, 3)
    # # END ;
    # # loadStringAdr(x);
    loadStringAdr(x)
    # # Put2(Ldr, RH, x.r, 0); Put1(Add, x.r, x.r, 4);
    Put2(Ldr, RH, x.r, 0)
    Put1(Add, x.r, x.r, 4)
    # # Put2(Str, RH, y.r, 0); Put1(Add, y.r, y.r, 4);
    Put2(Str, RH, y.r, 0)
    Put1(Add, y.r, y.r, 4)
    # # Put1(Asr, RH, RH, 24); Put3(BC, NE, -6); DEC(RH, 2)
    Put1(Asr, RH, RH, 24)
    Put3(BC, NE, -6)
    RH -= 2
# # END CopyString;

# Code generation for parameters

# # PROCEDURE VarParam*(VAR x: Item; ftype: ORB.Type);
def VarParam(x, ftype):
    # # VAR xmd: INTEGER;
    # # BEGIN xmd := x.mode; loadAdr(x);
    xmd = x.mode
    loadAdr(x)
    # # IF (ftype.form = ORB.Array) & (ftype.len < 0) THEN (*open array*)
    if (ftype.form == symbols.Array) and (ftype._len < 0):  # open array
        # # IF x.type.len >= 0 THEN Put1a(Mov, RH, 0, x.type.len) ELSE  Put2(Ldr, RH, SP, x.a+4+frame) END ;
        if x.type._len >= 0:
            Put1a(Mov, RH, 0, x.type.len)
        else:
            Put2(Ldr, RH, SP, x.a+4+frame)
        # # incR
        incR()
    # # ELSIF ftype.form = ORB.Record THEN
    elif ftype.form == symbols.Record:
        # # IF xmd = ORB.Par THEN Put2(Ldr, RH, SP, x.a+4+frame); incR ELSE loadTypTagAdr(x.type) END
        if xmd == symbols.Par:
            Put2(Ldr, RH, SP, x.a+4+frame)
            incR()
        else:
            loadTypTagAdr(x.type)
    # # END
# # END VarParam;

# # PROCEDURE ValueParam*(VAR x: Item);
def ValueParam(x):
    # # BEGIN load(x)
    load(x)
# # END ValueParam;

# # PROCEDURE OpenArrayParam*(VAR x: Item);
def OpenArrayParam(x):
    # # BEGIN loadAdr(x);
    loadAdr(x)
    # # IF x.type.len >= 0 THEN Put1a(Mov, RH, 0, x.type.len) ELSE Put2(Ldr, RH, SP, x.a+4+frame) END ;
    if x.type._len >= 0:
        Put1a(Mov, RH, 0, x.type._len)
    else:
        Put2(Ldr, RH, SP, x.a+4+frame)
    # # incR
    incR()
# # END OpenArrayParam;

# # PROCEDURE StringParam*(VAR x: Item);
def StringParam(x):
    # # BEGIN loadStringAdr(x); Put1(Mov, RH, 0, x.b); incR  (*len*)
    loadStringAdr(x)
    Put1(Mov, RH, 0, x.b)
    incR()  # len
# # END StringParam;

# For Statements

# # PROCEDURE For0*(VAR x, y: Item);
def For0(x, y):
    # # BEGIN load(y)
    load(y)
# # END For0;

# # PROCEDURE For1*(VAR x, y, z, w: Item; VAR L: LONGINT);
def For1(x, y, z, w, L):
    global RH
    # # BEGIN 
    # # IF z.mode = ORB.Const THEN Put1a(Cmp, RH, y.r, z.a)
    if z.mode == symbols.Const:
        Put1a(Cmp, RH, y.r, z.a)
    # # ELSE load(z); Put0(Cmp, RH-1, y.r, z.r); DEC(RH)
    else:
        load(z)
        Put0(Cmp, RH-1, y.r, z.r)
        RH -= 1
    # # END ;
    # # L := pc;
    L = len(code)
    # # IF w.a > 0 THEN Put3(BC, GT, 0)
    if w.a > 0:
        Put3(BC, GT, 0)
    elif w.a < 0:
        Put3(BC, LT, 0)
    # # ELSE ORS.Mark("zero increment"); Put3(BC, MI, 0)
    else:
        print "zero increment"
        Put3(BC, MI, 0)
    # # END ;
    # # Store(x, y)
    Store(x, y)
# # END For1;

# # PROCEDURE For2*(VAR x, y, w: Item);
def For2(x, y, w):
    global RH
    # # BEGIN load(x); DEC(RH); Put1a(Add, x.r, x.r, w.a)
    load(x)
    RH -= 1
    Put1a(Add, x.r, x.r, w.a)
# # END For2;

# Branches, procedure calls, procedure prolog and epilog

# # PROCEDURE Here*(): LONGINT;
def Here():
    # # BEGIN invalSB; RETURN pc
    invalSB()
    return len(code)
# # END Here;

# # PROCEDURE FJump*(VAR L: LONGINT);
def FJump(L):
    # # BEGIN Put3(BC, 7, L); L := pc-1
    Put3(BC, 7, L)
    L = len(code)-1
# # END FJump;

# # PROCEDURE CFJump*(VAR x: Item);
def CFJump(x):
    # # BEGIN
    # # IF x.mode # Cond THEN loadCond(x) END ;
    if x.mode != Cond:
        loadCond(x)
    # # Put3(BC, negated(x.r), x.a); FixLink(x.b); x.a := pc-1
    Put3(BC, negated(x.r), x.a)
    FixLink(x.b)
    x.a = len(code)-1
# # END CFJump;

# # PROCEDURE BJump*(L: LONGINT);
def BJump(L):
    # # BEGIN Put3(BC, 7, L-pc-1)
    Put3(BC, 7, L-len(code)-1)
# # END BJump;

# # PROCEDURE CBJump*(VAR x: Item; L: LONGINT);
def CBJump(x, L):
    # # BEGIN
    # # IF x.mode # Cond THEN loadCond(x) END ;
    if x.mode != Cond:
        loadCond(x)
    # # Put3(BC, negated(x.r), L-pc-1); FixLink(x.b); FixLinkWith(x.a, L)
    Put3(BC, negated(x.r), L-len(code)-1)
    FixLink(x.b)
    FixLinkWith(x.a, L)
# # END CBJump;

# # PROCEDURE Fixup*(VAR x: Item);
def Fixup(x):
    # # BEGIN FixLink(x.a)
    FixLink(x.a)
# # END Fixup;

# # PROCEDURE SaveRegs(r: LONGINT);  (* R[0 .. r-1]*)
def SaveRegs(r):  # R[0 .. r-1]
    # # VAR r0: LONGINT;
    # # BEGIN (*r > 0*) r0 := 0;
    # r > 0
    r0 = 0
    # # Put1(Sub, SP, SP, r*4); INC(frame, 4*r);
    Put1(Sub, SP, SP, r*4)
    frame += 4*r
    # # REPEAT Put2(Str, r0, SP, (r-r0-1)*4); INC(r0) UNTIL r0 = r
    while True:
        Put2(Str, r0, SP, (r-r0-1)*4)
        r0 += 1
        if r0 == r:
            break
# # END SaveRegs;

# # PROCEDURE RestoreRegs(r: LONGINT); (*R[0 .. r-1]*)
def RestoreRegs(r):  # R[0 .. r-1]
    # # VAR r0: LONGINT;
    # # BEGIN (*r > 0*) r0 := r;
    # r > 0
    r0 = r
    # # REPEAT DEC(r0); Put2(Ldr, r0, SP, (r-r0-1)*4) UNTIL r0 = 0;
    while True:
        r0 -= 1
        Put2(Ldr, r0, SP, (r-r0-1)*4)
        if r0 == 0:
            break
    # # Put1(Add, SP, SP, r*4); DEC(frame, 4*r)
    Put1(Add, SP, SP, r*4)
    frame -= 4*r
# # END RestoreRegs;

# # PROCEDURE PrepCall*(VAR x: Item; VAR r: LONGINT);
def PrepCall(x, r):
    global RH
    # # BEGIN (*x.type.form = ORB.Proc*)
    # x.type.form = symbols.Proc
    # # IF x.mode > ORB.Par THEN load(x) END ;
    if x.mode > symbols.Par:
        load(x)
    # # r := RH;
    r = RH
    # # IF RH > 0 THEN SaveRegs(RH); RH := 0 END
    if RH > 0:
        SaveRegs(RH)
        RH = 0
# # END PrepCall;

# # PROCEDURE Call*(VAR x: Item; r: LONGINT);
def Call(x, r):
    global RH
    # # BEGIN (*x.type.form = ORB.Proc*)
    # x.type.form = symbols.Proc
    # # IF x.mode = ORB.Const THEN
    if x.mode == symbols.Const:
        # # IF x.r >= 0 THEN Put3(BL, 7, (x.a DIV 4)-pc-1)
        if x.r >= 0:
           Put3(BL, 7, (x.a DIV 4)-len(code)-1)
        # # ELSE (*imported*)
        else:  # imported
            # # IF pc - fixorgP < 1000H THEN
            if len(code) - fixorgP < 0x1000:
                # # Put3(BL, 7, ((-x.r) * 100H + x.a) * 1000H + pc-fixorgP); fixorgP := pc-1
                Put3(BL, 7, ((-x.r) * 0x100 + x.a) * 0x1000 + pc-fixorgP)
                fixorgP = len(code)-1
            # # ELSE ORS.Mark("fixup impossible")
            else:
                print "fixup impossible"
            # # END
        # # END
    # # ELSE
    else:
        # # IF x.mode <= ORB.Par THEN load(x); DEC(RH)
        if x.mode <= symbols.Par:
            load(x)
            RH -= 1
        # # ELSE Put2(Ldr, RH, SP, 0); Put1(Add, SP, SP, 4); DEC(r); DEC(frame, 4)
        else:
            Put2(Ldr, RH, SP, 0)
            Put1(Add, SP, SP, 4)
            r -= 1
            frame -= 4
        # # END ;
        # # IF check THEN Trap(EQ, 5) END ;
        if check:
            Trap(EQ, 5)
        # # Put3(BLR, 7, RH)
        Put3(BLR, 7, RH)
    # # END ;
    # # IF x.type.base.form = ORB.NoTyp THEN (*procedure*) RH := 0
    if x.type.base.form == symbols.NoTyp:
        # procedure
        RH = 0
    # # ELSE (*function*)
    else:
        # function
        # # IF r > 0 THEN Put0(Mov, r, 0, 0); RestoreRegs(r) END ;
        if r > 0:
            Put0(Mov, r, 0, 0)
            RestoreRegs(r)
        # # x.mode := Reg; x.r := r; RH := r+1
        x.mode = Reg
        x.r = r
        RH = r+1
    # # END ;
    # # invalSB
    invalSB()
# # END Call;

# # PROCEDURE Enter*(parblksize, locblksize: LONGINT; int: BOOLEAN);
def Enter(parblksize, locblksize, int):
    # # VAR a, r: LONGINT;
    # # BEGIN invalSB; frame := 0;
    invalSB()
    frame = 0
    # # IF ~int THEN (*procedure prolog*)
    if !int:  # procedure prolog
        # # a := 4; r := 0;
        a = 4
        r = 0
        # # Put1(Sub, SP, SP, locblksize); Put2(Str, LNK, SP, 0);
        Put1(Sub, SP, SP, locblksize)
        Put2(Str, LNK, SP, 0)
        # # WHILE a < parblksize DO Put2(Str, r, SP, a); INC(r); INC(a, 4) END
        while a < parblksize:
            Put2(Str, r, SP, a)
            r += 1
            a += 4
    # # ELSE (*interrupt procedure*)
    else:  # interrupt procedure
        # # Put1(Sub, SP, SP, 12); Put2(Str, 0, SP, 0); Put2(Str, 1, SP, 4); Put2(Str, SB, SP, 8)
        Put1(Sub, SP, SP, 12)
        Put2(Str, 0, SP, 0)
        Put2(Str, 1, SP, 4)
        Put2(Str, SB, SP, 8)
        # # (*R0, R1, SB saved os stack*)
        # R0, R1, SB saved os stack
    # # END
# # END Enter;

# # PROCEDURE Return*(form: INTEGER; VAR x: Item; size: LONGINT; int: BOOLEAN);
def Return(form, x, size, int):
    global RH
    # # BEGIN
    # # IF form # ORB.NoTyp THEN load(x) END ;
    if form != symbols.NoTyp:
        load(x)
    # # IF ~int THEN (*procedure epilog*)
    if !int:  # procedure epilog
        Put2(Ldr, LNK, SP, 0)
        Put1(Add, SP, SP, size)
        Put3(BR, 7, LNK)
    # # ELSE (*interrupt return, restore SB, R1, R0*)
    else:  # interrupt return, restore SB, R1, R0
        # # Put2(Ldr, SB, SP, 8); Put2(Ldr, 1, SP, 4); Put2(Ldr, 0, SP, 0); Put1(Add, SP, SP, 12); Put3(BR, 7, 10H)
        Put2(Ldr, SB, SP, 8)
        Put2(Ldr, 1, SP, 4)
        Put2(Ldr, 0, SP, 0)
        Put1(Add, SP, SP, 12)
        Put3(BR, 7, 10H)
    # # END ;
    # # RH := 0
    RH = 0
# # END Return;

# In-line code procedures

# # PROCEDURE Increment*(upordown: LONGINT; VAR x, y: Item);
def Increment(upordown, x, y):
    global RH
    # # VAR op, zr, v: LONGINT;
    # # BEGIN (*frame = 0*)
    # frame = 0
    # # IF upordown = 0 THEN op := Add ELSE op := Sub END ;
    if upordown == 0:
        op = Add
    else:
        op = Sub
    # # IF x.type = ORB.byteType THEN v := 1 ELSE v := 0 END ;
    if x.type == symbols.byteType:
        v = 1
    else:
        v = 0
    # # IF y.type.form = ORB.NoTyp THEN y.mode := ORB.Const; y.a := 1 END ;
    if y.type.form == symbols.NoTyp:
        y.mode = symbols.Const
    else:
        y.a := 1
    # # IF (x.mode = ORB.Var) & (x.r > 0) THEN
    if (x.mode == symbols.Var) and (x.r > 0):
        # # zr := RH; Put2(Ldr+v, zr, SP, x.a); incR;
        zr = RH
        Put2(Ldr+v, zr, SP, x.a)
        incR()
        # # IF y.mode = ORB.Const THEN Put1a(op, zr, zr, y.a) ELSE load(y); Put0(op, zr, zr, y.r); DEC(RH) END ;
        if y.mode == symbols.Const:
            Put1a(op, zr, zr, y.a)
        else:
            load(y)
            Put0(op, zr, zr, y.r)
            RH -= 1
        # # Put2(Str+v, zr, SP, x.a); DEC(RH)
        Put2(Str+v, zr, SP, x.a)
        RH -= 1
    # # ELSE loadAdr(x); zr := RH; Put2(Ldr+v, RH, x.r, 0); incR;
    else:
        loadAdr(x)
        zr = RH
        Put2(Ldr+v, RH, x.r, 0)
        incR()
        # # IF y.mode = ORB.Const THEN Put1a(op, zr, zr, y.a) ELSE load(y); Put0(op, zr, zr, y.r); DEC(RH) END ;
        if y.mode == symbols.Const:
            Put1a(op, zr, zr, y.a)
        else:
            load(y)
            Put0(op, zr, zr, y.r)
            RH -= 1
        # # Put2(Str+v, zr, x.r, 0); DEC(RH, 2)
        Put2(Str+v, zr, x.r, 0)
        RH -= 2
    # # END
# # END Increment;

# # PROCEDURE Include*(inorex: LONGINT; VAR x, y: Item);
def Include(inorex, x, y):
    global RH
    # # VAR op, zr: LONGINT;
    # # BEGIN loadAdr(x); zr := RH; Put2(Ldr, RH, x.r, 0); incR;
    loadAdr(x)
    zr = RH
    Put2(Ldr, RH, x.r, 0)
    incR()
    # # IF inorex = 0 THEN op := Ior ELSE op := Ann END ;
    if inorex == 0:
        op = Ior
    else:
        op = Ann
    # # IF y.mode = ORB.Const THEN Put1a(op, zr, zr, LSL(1, y.a))
    if y.mode == symbols.Const:
        Put1a(op, zr, zr, LSL(1, y.a))
    # # ELSE load(y); Put1(Mov, RH, 0, 1); Put0(Lsl, y.r, RH, y.r); Put0(op, zr, zr, y.r); DEC(RH)
    else:
        load(y)
        Put1(Mov, RH, 0, 1)
        Put0(Lsl, y.r, RH, y.r)
        Put0(op, zr, zr, y.r)
        RH -= 1
    # # END ;
    # # Put2(Str, zr, x.r, 0); DEC(RH, 2)
    Put2(Str, zr, x.r, 0)
    RH -= 2
# # END Include;

# # PROCEDURE Assert*(VAR x: Item);
def Assert(x):
    # # VAR cond: LONGINT;
    # # BEGIN
    # # IF x.mode # Cond THEN loadCond(x) END ;
    if x.mode != Cond:
        loadCond(x)
    # # IF x.a = 0 THEN cond := negated(x.r)
    if x.a == 0:
        cond = negated(x.r)
    # # ELSE Put3(BC, x.r, x.b); FixLink(x.a); x.b := pc-1; cond := 7
    else:
        Put3(BC, x.r, x.b)
        FixLink(x.a)
        x.b = len(code)-1
        cond = 7
    # # END ;
    # # Trap(cond, 7); FixLink(x.b)
    Trap(cond, 7)
    FixLink(x.b)
# # END Assert; 

# # PROCEDURE New*(VAR x: Item);
def New(x):
    global RH
    # # BEGIN loadAdr(x); loadTypTagAdr(x.type.base); Put3(BLR, 7, MT); RH := 0; invalSB
    loadAdr(x)
    loadTypTagAdr(x.type.base)
    Put3(BLR, 7, MT)
    RH = 0
    invalSB()
# # END New;

# # PROCEDURE Pack*(VAR x, y: Item);
def Pack(x, y):
    global RH
    # # VAR z: Item;
    # # BEGIN z := x; load(x); load(y);
    z = x
    load(x)
    load(y)
    # # Put1(Lsl, y.r, y.r, 23); Put0(Add, x.r, x.r, y.r); DEC(RH); Store(z, x)
    Put1(Lsl, y.r, y.r, 23)
    Put0(Add, x.r, x.r, y.r)
    RH -= 1
    Store(z, x)
# # END Pack;

# # PROCEDURE Unpk*(VAR x, y: Item);
def Unpk(x, y):
    # # VAR z, e0: Item;
    # # BEGIN  z := x; load(x); e0.mode := Reg; e0.r := RH; e0.type := ORB.intType;
    z = x
    load(x)
    e0.mode = Reg
    e0.r = RH
    e0.type = symbols.intType
    # # Put1(Asr, RH, x.r, 23); Put1(Sub, RH, RH, 127); Store(y, e0); incR;
    Put1(Asr, RH, x.r, 23)
    Put1(Sub, RH, RH, 127)
    Store(y, e0)
    incR()
    # # Put1(Lsl, RH, RH, 23); Put0(Sub, x.r, x.r, RH); Store(z, x)
    Put1(Lsl, RH, RH, 23)
    Put0(Sub, x.r, x.r, RH)
    Store(z, x)
# # END Unpk;

# # PROCEDURE Led*(VAR x: Item);
def Led(x):
    global RH
    # # BEGIN load(x); Put1(Mov, RH, 0, -60); Put2(Str, x.r, RH, 0); DEC(RH)
    load(x)
    Put1(Mov, RH, 0, -60)
    Put2(Str, x.r, RH, 0)
    RH -= 1
# # END Led;

# # PROCEDURE Get*(VAR x, y: Item);
def Get(x, y):
    # # BEGIN load(x); x.type := y.type; x.mode := RegI; x.a := 0; Store(y, x)
    load(x)
    x.type = y.type
    x.mode = RegI
    x.a = 0
    Store(y, x)
# # END Get;

# # PROCEDURE Put*(VAR x, y: Item);
def Put(x, y):
    # # BEGIN load(x); x.type := y.type; x.mode := RegI; x.a := 0; Store(x, y)
    load(x)
    x.type = y.type
    x.mode = RegI
    x.a = 0
    Store(x, y)
# # END Put;

# # PROCEDURE Copy*(VAR x, y, z: Item);
def Copy(x, y, z):
    global RH
    # # BEGIN load(x); load(y);
    load(x)
    load(y)
    # # IF z.mode = ORB.Const THEN
    if z.mode == symbols.Const:
        # # IF z.a > 0 THEN load(z) ELSE ORS.Mark("bad count") END
        if z.a > 0:
            load(z)
        else:
            print "bad count"
    # # ELSE load(z);
    else:
        load(z);
        # # IF check THEN Trap(LT, 3) END ;
        if check:
            Trap(LT, 3)
        # # Put3(BC, EQ, 6)
        Put3(BC, EQ, 6)
    # # END ;
    # # Put2(Ldr, RH, x.r, 0); Put1(Add, x.r, x.r, 4);
    Put2(Ldr, RH, x.r, 0)
    Put1(Add, x.r, x.r, 4)
    # # Put2(Str, RH, y.r, 0); Put1(Add, y.r, y.r, 4);
    Put2(Str, RH, y.r, 0)
    Put1(Add, y.r, y.r, 4)
    # # Put1(Sub, z.r, z.r, 1); Put3(BC, NE, -6); DEC(RH, 3)
    Put1(Sub, z.r, z.r, 1)
    Put3(BC, NE, -6)
    RH -= 3
# # END Copy;

# # PROCEDURE LDPSR*(VAR x: Item);
def LDPSR(x):
    # # BEGIN (*x.mode = Const*)  Put3(0, 15, x.a + 20H)
    # x.mode = Const
    Put3(0, 15, x.a + 0x20)
# # END LDPSR;

# # PROCEDURE LDREG*(VAR x, y: Item);
def LDREG(x, y):
    global RH
    # # BEGIN
    # # IF y.mode = ORB.Const THEN Put1a(Mov, x.a, 0, y.a)
    if y.mode == symbols.Const:
        Put1a(Mov, x.a, 0, y.a)
    # # ELSE load(y); Put0(Mov, x.a, 0, y.r); DEC(RH)
    else:
        load(y)
        Put0(Mov, x.a, 0, y.r)
        RH -= 1
    # # END
# # END LDREG;

# In-line code functions

# # PROCEDURE Abs*(VAR x: Item);
def Abs(x):
    # # BEGIN
    # # IF x.mode = ORB.Const THEN x.a := ABS(x.a)
    if x.mode == symbols.Const:
        x.a = abs(x.a)
    # # ELSE load(x);
    else:
        load(x)
        # # IF x.type.form = ORB.Real THEN Put1(Lsl, x.r, x.r, 1); Put1(Ror, x.r, x.r, 1)
        if x.mode == symbols.Real:
            Put1(Lsl, x.r, x.r, 1)
            Put1(Ror, x.r, x.r, 1)
        # # ELSE Put1(Cmp, x.r, x.r, 0); Put3(BC, GE, 2); Put1(Mov, RH, 0, 0); Put0(Sub, x.r, RH, x.r)
        else:
            Put1(Cmp, x.r, x.r, 0)
            Put3(BC, GE, 2)
            Put1(Mov, RH, 0, 0)
            Put0(Sub, x.r, RH, x.r)
        # # END
    # # END
# # END Abs;

# # PROCEDURE Odd*(VAR x: Item);
def Odd(x):
    global RH
    # # BEGIN load(x); Put1(And, x.r, x.r, 1); SetCC(x, NE); DEC(RH)
    load(x)
    Put1(And, x.r, x.r, 1)
    SetCC(x, NE)
    RH -= 1
# # END Odd;

# # PROCEDURE Floor*(VAR x: Item);
def Floor(x):
    # # BEGIN load(x); Put1(Mov+U, RH, 0, 4B00H); Put0(Fad+V, x.r, x.r, RH)
    load(x)
    Put1(Mov+U, RH, 0, 0x4B00)
    Put0(Fad+V, x.r, x.r, RH)
# # END Floor;

# # PROCEDURE Float*(VAR x: Item);
    # # BEGIN load(x); Put1(Mov+U, RH, 0, 4B00H);  Put0(Fad+U, x.r, x.r, RH)
    load(x)
    Put1(Mov+U, RH, 0, 0x4B00)
    Put0(Fad+U, x.r, x.r, RH)
# # END Float;

# # PROCEDURE Ord*(VAR x: Item);
def Ord(x):
    # # BEGIN
    # # IF x.mode IN {ORB.Var, ORB.Par, RegI, Cond} THEN load(x) END
    if x.mode in (symbols.Var, symbols.Par, RegI, Cond):
        load(x)
# # END Ord;

# # PROCEDURE Len*(VAR x: Item);
def Len(x):
    # # BEGIN
    # # IF x.type.len >= 0 THEN x.mode := ORB.Const; x.a := x.type.len
    if x.type._len >= 0:
        x.mode = symbols.Const
        x.a = x.type._len
    # # ELSE (*open array*) Put2(Ldr, RH, SP, x.a + 4 + frame); x.mode := Reg; x.r := RH; incR
    else:  # open array
        Put2(Ldr, RH, SP, x.a + 4 + frame)
        x.mode = Reg
        x.r = RH
        incR()
    # # END 
# # END Len;

# # PROCEDURE Shift*(fct: LONGINT; VAR x, y: Item);
def Shift(fct, x, y):
    global RH
    # # VAR op: LONGINT;
    # # BEGIN load(x);
    load(x)
    # # IF fct = 0 THEN op := Lsl ELSIF fct = 1 THEN op := Asr ELSE op := Ror END ;
    if fct == 0:
        op = Lsl
    elif fct == 1:
        op = Asr
    else:
        op = Ror
    # # IF y.mode = ORB.Const THEN Put1(op, x.r, x.r, y.a MOD 20H)
    if y.mode == symbols.Const:
        Put1(op, x.r, x.r, y.a MOD 0x20)
    # # ELSE load(y); Put0(op, RH-2, x.r, y.r); DEC(RH); x.r := RH-1
    else:
        load(y)
        Put0(op, RH-2, x.r, y.r)
        RH -= 1
        x.r = RH-1
    # # END
# # END Shift;

# # PROCEDURE ADC*(VAR x, y: Item);
def ADC(x, y):
    global RH
    # # BEGIN load(x); load(y); Put0(Add+2000H, x.r, x.r, y.r); DEC(RH)
    load(x)
    load(y)
    Put0(Add+0x2000, x.r, x.r, y.r)
    RH -= 1
# # END ADC;

# # PROCEDURE SBC*(VAR x, y: Item);
def SBC(x, y):
    global RH
    # # BEGIN load(x); load(y); Put0(Sub+2000H, x.r, x.r, y.r); DEC(RH)
    load(x)
    load(y)
    Put0(Sub+0x2000, x.r, x.r, y.r)
    RH -= 1
# # END SBC;

# # PROCEDURE UML*(VAR x, y: Item);
def UML(x, y):
    global RH
    # # BEGIN load(x); load(y); Put0(Mul+2000H, x.r, x.r, y.r); DEC(RH)
    load(x)
    load(y)
    Put0(Mul+0x2000, x.r, x.r, y.r)
    RH -= 1
# # END UML;

# # PROCEDURE Bit*(VAR x, y: Item);
def Bit(x, y):
    global RH
    # # BEGIN load(x); Put2(Ldr, x.r, x.r, 0);
    load(x)
    # # IF y.mode = ORB.Const THEN Put1(Ror, x.r, x.r, y.a+1); DEC(RH)
    if y.mode == symbols.Const:
        Put1(Ror, x.r, x.r, y.a+1)
        RH -= 1
    # # ELSE load(y); Put1(Add, y.r, y.r, 1); Put0(Ror, x.r, x.r, y.r); DEC(RH, 2)
    else:
        load(y)
        Put1(Add, y.r, y.r, 1)
        Put0(Ror, x.r, x.r, y.r)
        RH -= 2
    # # END ;
    # # SetCC(x, MI)
    SetCC(x, MI)
# # END Bit;

# # PROCEDURE Register*(VAR x: Item);
def Register(x):
    # # BEGIN (*x.mode = Const*)
    # x.mode = Const
    # # Put0(Mov, RH, 0, x.a MOD 10H); x.mode := Reg; x.r := RH; incR
    Put0(Mov, RH, 0, x.a % 0x10)
    x.mode = Reg
    x.r = RH
    incR()
# # END Register;

# # PROCEDURE H*(VAR x: Item);
def H(x):
    # # BEGIN (*x.mode = Const*)
    # x.mode = Const
    # # Put0(Mov + U + x.a MOD 2 * V, RH, 0, 0); x.mode := Reg; x.r := RH; incR
    Put0(Mov + U + x.a % 2 * V, RH, 0, 0)
    x.mode = Reg
    x.r = RH
    incR()
# # END H;

# # PROCEDURE Adr*(VAR x: Item);
def Adr(x):
    # # BEGIN 
    # # IF x.mode IN {ORB.Var, ORB.Par, RegI} THEN loadAdr(x)
    if x.mode in (symbols.Var, symbols.Par, RegI):
        load(x)
    # # ELSIF (x.mode = ORB.Const) & (x.type.form = ORB.Proc) THEN load(x)
    elif (x.mode == symbols.Const) and (x.type.form == symbols.Proc):
        load(x)
    # # ELSIF (x.mode = ORB.Const) & (x.type.form = ORB.String) THEN loadStringAdr(x)
    elif (x.mode == symbols.Const) and (x.type.form == symbols.String):
        loadStringAdr(x)
    # # ELSE ORS.Mark("not addressable")
    else:
        print "not addressable"
    # # END
# # END Adr;

# # PROCEDURE Condition*(VAR x: Item);
def Condition(x):
    # # BEGIN (*x.mode = Const*)
    # x.mode = Const
    SetCC(x, x.a)
# # END Condition;

# FIXME
# # PROCEDURE Open*(v: INTEGER);
def Open(v):
    global code, RH
    # # BEGIN pc := 0; tdx := 0; strx := 0; RH := 0; fixorgP := 0; fixorgD := 0; fixorgT := 0; check := v # 0; version := v;
    # pc = 0
    code = []
    tdx = 0
    strx = 0
    RH = 0
    fixorgP = 0
    fixorgD = 0
    fixorgT = 0
    check = (v != 0)
    version = v
    # # IF v = 0 THEN pc := 8 END
    if v == 0:
        code = 8*[0]
# # END Open;

# # PROCEDURE SetDataSize*(dc: LONGINT);
def SetDataSize(dc):
    global varsize
    # # BEGIN varsize := dc
    varsize = dc
# # END SetDataSize;

# # PROCEDURE Header*;
def Header():
    # # BEGIN entry := pc*4;
    entry = len(code)*4
    # # IF version = 0 THEN code[0] := 0E7000000H-1 + pc; Put1a(Mov, SB, 0, VarOrg0); Put1a(Mov, SP, 0, StkOrg0)  (*RISC-0*)
    if version == 0:
        code[0] = 0x0E7000000-1 + len(code)
        Put1a(Mov, SB, 0, VarOrg0)
        Put1a(Mov, SP, 0, StkOrg0)  # RISC-0
    # # ELSE Put1(Sub, SP, SP, 4); Put2(Str, LNK, SP, 0); invalSB
    else:
        Put1(Sub, SP, SP, 4)
        Put2(Str, LNK, SP, 0)
        invalSB()
    # # END
# # END Header;

# # PROCEDURE NofPtrs(typ: ORB.Type): LONGINT;
def NofPtrs(typ):
    # # VAR fld: ORB.Object; n: LONGINT;
    # # BEGIN
    # # IF (typ.form = ORB.Pointer) OR (typ.form = ORB.NilTyp) THEN n := 1
    if (typ.form == symbols.Pointer) or (typ.form == symbols.NilTyp):
        n = 1
    # # ELSIF typ.form = ORB.Record THEN
    elif typ.form == symbols.Record:
        # # fld := typ.dsc; n := 0;
        fld = typ.dsc
        n = 0
        # # WHILE fld # NIL DO n := NofPtrs(fld.type) + n; fld := fld.next END
        while fld != None:
            n += NofPtrs(fld.type)
            fld = fld.next
    # # ELSIF typ.form = ORB.Array THEN n := NofPtrs(typ.base) * typ.len
    elif typ.form == symbols.Array:
        n = NofPtrs(typ.base) * typ._len
    # # ELSE n := 0
    else:
        n = 0
    # # END ;
    # # RETURN n
    return n
# # END NofPtrs;

# # PROCEDURE FindPtrs(VAR R: Files.Rider; typ: ORB.Type; adr: LONGINT);
def FindPtrs(f, typ, adr):
    # # VAR fld: ORB.Object; i, s: LONGINT;
    # # BEGIN
    # # IF (typ.form = ORB.Pointer) OR (typ.form = ORB.NilTyp) THEN Files.WriteInt(R, adr)
    if (typ.form == symbols.Pointer) or (typ.form == symbols.NilTyp):
        f.write_int(adr)
    # # ELSIF typ.form = ORB.Record THEN
    elif typ.form == symbols.Record:
        # # fld := typ.dsc;
        fld = typ.dsc
        # # WHILE fld # NIL DO FindPtrs(R, fld.type, fld.val + adr); fld := fld.next END
        while fld != None:
            FindPtrs(f, fld.type, fld.val + adr)
            fld = fld.next
    # # ELSIF typ.form = ORB.Array THEN
    elif typ.form == symbols.Array:
        # # s := typ.base.size;
        s = typ.base.size
        # # FOR i := 0 TO typ.len-1 DO FindPtrs(R, typ.base, i*s + adr) END
        for i in range(typ._len):
            FindPtrs(f, typ.base, i*s + adr)
    # # END
# # END FindPtrs;

# # PROCEDURE Close*(VAR modid: ORS.Ident; key, nofent: LONGINT);
def Close(modid, key, nofent):
    # # VAR obj: ORB.Object;
      # # i, comsize, nofimps, nofptrs, size: LONGINT;
      # # name: ORS.Ident;
      # # F: Files.File; R: Files.Rider;
    # # BEGIN  (*exit code*)
    # exit code
    # # IF version = 0 THEN Put1(Mov, 0, 0, 0); Put3(BR, 7, 0)  (*RISC-0*)
    if version == 0:
        Put1(Mov, 0, 0, 0)
        Put3(BR, 7, 0)  # RISC-0
    # # ELSE Put2(Ldr, LNK, SP, 0); Put1(Add, SP, SP, 4); Put3(BR, 7, LNK)
    else:
        Put2(Ldr, LNK, SP, 0)
        Put1(Add, SP, SP, 4)
        Put3(BR, 7, LNK)
    # # END ;
    # # obj := ORB.topScope.next; nofimps := 0; comsize := 4; nofptrs := 0;
    nofimps = 0
    comsize = 4
    nofptrs = 0
    obj = symbols.topScope.next
    # # WHILE obj # NIL DO
    while obj != None:
        # # IF (obj.class = ORB.Mod) & (obj.dsc # ORB.system) THEN INC(nofimps) (*count imports*)
        if (obj.klass == symbols.Mod) and (obj.dsc != symbols.system):
            nofimps += 1 # count imports
        # # ELSIF (obj.exno # 0) & (obj.class = ORB.Const) & (obj.type.form = ORB.Proc)
            # # & (obj.type.nofpar = 0) & (obj.type.base = ORB.noType) THEN i := 0; (*count commands*)
        elif (obj.exno != 0) and (obj.klass == symbols.Const) and (obj.type.form == symbols.Proc) \
            and (obj.type.nofpar == 0) and (obj.type.base == symbols.noType):
            i = 0 # count commands
            # # WHILE obj.name[i] # 0X DO INC(i) END ;
            while obj.name[i] != '\0':
                i += 1
            # # i := (i+4) DIV 4 * 4; INC(comsize, i+4)
            i = (i+4) / 4 * 4
            comsize += i+4
        # # ELSIF obj.class = ORB.Var THEN INC(nofptrs, NofPtrs(obj.type))  (*count pointers*)
        elif obj.klass == symbols.Var:
            nofptrs += NofPtrs(obj.type)  # count pointers
        # # END ;
        # # obj := obj.next
        obj = obj.next
    # # END ;
    # # size := varsize + strx + comsize + (pc + nofimps + nofent + nofptrs + 1)*4;  (*varsize includes type descriptors*)
    size = varsize + strx + comsize + (len(code) + nofimps + nofent + nofptrs + 1)*4  # varsize includes type descriptors

    # # ORB.MakeFileName(name, modid, ".rsc"); (*write code file*)
    name = modid + ".rsc"  # write code file
    # # F := Files.New(name); Files.Set(R, F, 0); Files.WriteString(R, modid); Files.WriteInt(R, key); Files.WriteByte(R, version);
    f = SerializingWriter(name)
    f.write_string(modid)
    f.write_int(key)
    f.write_byte(version)
    # # Files.WriteInt(R, size);
    f.write_int(size)
    # # obj := ORB.topScope.next;
    obj = symbols.topScope.next
    # # WHILE (obj # NIL) & (obj.class = ORB.Mod) DO  (*imports*)
    while (obj != None) and (obj.klass == symbols.Mod):  # imports
        # # IF obj.dsc # ORB.system THEN Files.WriteString(R, obj(ORB.Module).orgname); Files.WriteInt(R, obj.val) END ;
        if obj.dsc != symbols.system:
            f.write_string(obj(symbols.Module).orgname)
            f.write_int(obj.val)
        # # obj := obj.next
        obj = obj.next
    # # END ;
    # # Files.Write(R, 0X);
    f.write('\0')
    # # Files.WriteInt(R, tdx*4);
    f.write_int(tdx*4)
    # # i := 0;
    i = 0
    # # WHILE i < tdx DO Files.WriteInt(R, data[i]); INC(i) END ; (*type descriptors*)
    while i < tdx:
        f.write_int(data[i])
        i += 1  # type descriptors
    # # Files.WriteInt(R, varsize - tdx*4);  (*data*)
    f.write_int(varsize - tdx*4)  # data
    # # Files.WriteInt(R, strx);
    f.write_int(strx)
    # # FOR i := 0 TO strx-1 DO Files.Write(R, str[i]) END ;  (*strings*)
    for i in range(strx):
        f.write(str[i])  # strings
    # # Files.WriteInt(R, pc);  (*code len*)
    f.write_int(len(code))  # code len
    # # FOR i := 0 TO pc-1 DO Files.WriteInt(R, code[i]) END ;  (*program*)
    for i in code:
        f.write_int(i)  # program
    # # obj := ORB.topScope.next;
    obj = symbols.topScope.next
    # # WHILE obj # NIL DO  (*commands*)
    while obj != None:  # commands
        # # IF (obj.exno # 0) & (obj.class = ORB.Const) & (obj.type.form = ORB.Proc) &
            # # (obj.type.nofpar = 0) & (obj.type.base = ORB.noType) THEN
        if (obj.exno != 0) and (obj.klass == symbols.Const) and (obj.type.form == symbols.Proc) and \
            (obj.type.nofpar == 0) and (obj.type.base == symbols.noType):
            # # Files.WriteString(R, obj.name); Files.WriteInt(R, obj.val)
            f.write_string(obj.name)
            f.write_int(obj.val)
        # # END ;
        # # obj := obj.next
        obj = obj.next
    # # END ;
    # # Files.Write(R, 0X);
    f.write(R, '\0')
    # # Files.WriteInt(R, nofent); Files.WriteInt(R, entry);
    f.write_int(nofent)
    f.write_int(entry)
    # # obj := ORB.topScope.next;
    obj = symbols.topScope.next
    # # WHILE obj # NIL DO  (*entries*)
    while obj != None:  # entries
        # # IF obj.exno # 0 THEN
        if obj.exno != 0:
            # # IF (obj.class = ORB.Const) & (obj.type.form = ORB.Proc) OR (obj.class = ORB.Var) THEN
            if (obj.klass == symbols.Const) and (obj.type.form == symbols.Proc) or (obj.klass == symbols.Var):
                # # Files.WriteInt(R, obj.val)
                f.write_int(obj.val)
            # # ELSIF obj.class = ORB.Typ THEN
            elif obj.klass == symbols.Typ:
                # # IF obj.type.form = ORB.Record THEN Files.WriteInt(R,  obj.type.len MOD 10000H)
                if obj.type.form == symbols.Record:
                    f.write_int(obj.type.len % 0x10000)
                # # ELSIF (obj.type.form = ORB.Pointer) & ((obj.type.base.typobj = NIL) OR (obj.type.base.typobj.exno = 0)) THEN
                elif (obj.type.form == symbols.Pointer) and ((obj.type.base.typobj == None) or (obj.type.base.typobj.exno == 0)):
                    # # Files.WriteInt(R, obj.type.base.len MOD 10000H)
                    f.write_int(obj.type.base.len % 0x10000)
                # # END
            # # END
        # # END ;
        # # obj := obj.next
        obj = obj.next
    # # END ;
    # # obj := ORB.topScope.next;
    obj = symbols.topScope.next
    # # WHILE obj # NIL DO  (*pointer variables*)
    while obj != None:  # pointer variables
        # # IF obj.class = ORB.Var THEN FindPtrs(R, obj.type, obj.val) END ;
        if obj.klass == symbols.Var:
            FindPtrs(f, obj.type, obj.val)
        # # obj := obj.next
    # # END ;
    # # Files.WriteInt(R, -1);
    f.write_int(-1)
    # # Files.WriteInt(R, fixorgP); Files.WriteInt(R, fixorgD); Files.WriteInt(R, fixorgT); Files.WriteInt(R, entry);
    f.write_int(fixorgP)
    f.write_int(fixorgD)
    f.write_int(fixorgT)
    f.write_int(entry)
    # # Files.Write(R, "O"); Files.Register(F)
    f.write(R, 'O')
    f.close()
# # END Close;

# # BEGIN
    # # relmap[0] := 1; relmap[1] := 9; relmap[2] := 5; relmap[3] := 6; relmap[4] := 14; relmap[5] := 13
# # END ORG.
