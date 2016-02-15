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


# tools.py - Python version of ORTool.Mod.Txt (NW 18.2.2013)

# This version lets one view the object file in human
# readable format.

# Note: I elect to omit a few little routines, plus
# recode the logic a bit to make it more Pythonic
# and UNIXish.

import symbols

# # PROCEDURE ReadType(VAR R: Files.Rider);
    # # VAR key, len, lev, size, off: INTEGER;
      # # ref, mno, class, form, readonly: INTEGER;
      # # name, modname: ARRAY 32 OF CHAR;
def read_type(f):
    # read in type description
    # # BEGIN Read(R, ref); Texts.Write(W, " "); Texts.Write(W, "[");
    # # IF ref < 0 THEN Texts.Write(W, "^"); Texts.WriteInt(W, -ref, 1)
    ref = f.read()
    if ref < 0:
        t = ' [^%1d]' % (-ref,)
    # # ELSE Texts.WriteInt(W, ref, 1);
    else:
        # # Read(R, form); Texts.WriteString(W, "  form = "); Texts.WriteInt(W, form, 1);
        form = f.read()
        t = ' [%1d  form = %1d' % (ref,form)
        # # IF form = ORB.Pointer THEN ReadType(R)
        if form == symbols.Pointer:
            t += read_type(f)
        # # ELSIF form = ORB.Array THEN
        elif form == symbols.Array:
            # # ReadType(R); Files.ReadNum(R, len); Files.ReadNum(R, size);
            # # Texts.WriteString(W, "  len = "); Texts.WriteInt(W, len, 1);
            # # Texts.WriteString(W, "  size = "); Texts.WriteInt(W, size, 1)
            t += read_type(f)
            length = f.read_num()
            size = f.read_num()
            t += '  len = %1d  size = %1d' % (length,size)
        # # ELSIF form = ORB.Record THEN
        elif form == symbols.Record:
            # # ReadType(R);  (*base type*)
            # # Files.ReadNum(R, off); Texts.WriteString(W, "  exno = "); Texts.WriteInt(W, off, 1); 
            # # Files.ReadNum(R, off); Texts.WriteString(W, "  extlev = "); Texts.WriteInt(W, off, 1);
            # # Files.ReadNum(R, size); Texts.WriteString(W, "  size = "); Texts.WriteInt(W, size, 1);
            # # Texts.Write(W, " "); Texts.Write(W, "{"); Read(R, class);
            t += read_type(f)  # base type
            off = f.read_num()
            extlev = f.read_num()
            size = f.read_num()
            t += '  exno = %1d  extlev = %1d  size = %1d {' % (off,extlev,size)
            # # WHILE class # 0 DO (*fields*)
            while True:  # fields
                klass = f.read()
                if klass == 0:
                    break
                # # Files.ReadString(R, name);
                name = f.read_string()
                # # IF name[0] # 0X THEN Texts.Write(W, " "); Texts.WriteString(W, name); ReadType(R)
                if name != '':
                    t += ' '+name+read_type(f)
                # # ELSE Texts.WriteString(W, " --")
                else:
                    t += ' --'
                # # END ;
                # # Files.ReadNum(R, off); Texts.WriteInt(W, off, 4); Read(R, class)
                t += '%4d' % (f.read_num(),)
            # # END ;
            # # Texts.Write(W, "}")
            t += '}'
        # # ELSIF form = ORB.Proc THEN
        elif form == symbols.Array:
            # # ReadType(R); Texts.Write(W, "("); Read(R, class);
            t += read_type(f)+'('
            # # WHILE class # 0 DO
            while True:
                klass = f.read()
                if klass == 0:
                    break
                # # Texts.WriteString(W, " class = "); Texts.WriteInt(W, class, 1); Read(R, readonly);
                t += ' class = %1d' % (klass,)
                readonly = f.read()
                # # IF readonly = 1 THEN Texts.Write(W, "#") END ;
                if readonly == 1:
                    t += '#'
                # # ReadType(R); Read(R, class)
                t += read_type(f)
            # # END ;
            # # Texts.Write(W, ")")
            t += ')'
        # # END ;
        # # Files.ReadString(R, modname);
        modname = f.read_string()
        # # IF modname[0] # 0X THEN
        if modname != '':
            # # Files.ReadInt(R, key); Files.ReadString(R, name);
            # # Texts.Write(W, " "); Texts.WriteString(W, modname); Texts.Write(W, "."); Texts.WriteString(W, name);
            # # Texts.WriteHex(W, key)
            key = f.read_int()
            name = f.read_string()
            t += ' %s.%s%08X' % (modname,name,key)
        # # END
        t += ']'
    # # END ;
    # # Form := form; Texts.Write(W, "]")
    # # END ReadType;

# # PROCEDURE DecSym*;  (*decode symbol file*)
def decode_symbol(fname):  # decode symbol file
    # # VAR class, typno, k: INTEGER;
      # # name: ARRAY 32 OF CHAR;
      # # F: Files.File; R: Files.Rider;
      # # S: Texts.Scanner;
  # # BEGIN Texts.OpenScanner(S, Oberon.Par.text, Oberon.Par.pos); Texts.Scan(S);
    # # IF S.class = Texts.Name THEN
    # # Texts.WriteString(W, "OR-decode "); Texts.WriteString(W, S.s);
    # # Texts.WriteLn(W); Texts.Append(Oberon.Log, W.buf);
    print 'OR-decode', fname
    # # F := Files.Old(S.s);
    f = symbols.SerializingReader(fname)
    # # IF F # NIL THEN ...
    # # ELSE Texts.WriteString(W, " not found")
    # # END ;
    if f.content == None:
        f.close()
        print '%s not found' % (fname,)
        return
    print
    # # Files.Set(R, F, 0); Files.ReadInt(R, k); Files.ReadInt(R, k);
    k = f.read_int()
    k = f.read_int()
    # # Files.ReadString(R, name); Texts.WriteString(W, name); Texts.WriteHex(W, k);
    # # Read(R, class); Texts.WriteInt(W, class, 3); (*sym file version*)
    name = f.read_string()
    klass = f.read()  # version
    t = '%s %08X %3d' % (name,k,klass)
    # # IF class = ORB.versionkey THEN ...
    # # ELSE Texts.WriteString(W, " bad symfile version")
    # # END
    if klass != symbols.versionkey:
        f.close()
        print t+' bad symfile version'
        return
    print t
    # # Texts.WriteLn(W); Read(R, class);
    # # WHILE class # 0 DO
    while True:
        klass = f.read()
        if klass == 0:
            break
        # # Texts.WriteInt(W, class, 4); Files.ReadString(R, name); Texts.Write(W, " "); Texts.WriteString(W, name);
        name = f.read_string()
        print ' %4d %s' % (klass,name)
        # # ReadType(R);
        t = read_type(f)
        # # IF class = ORB.Typ THEN
        # #   Texts.Write(W, "("); Read(R, class);
        # #   WHILE class # 0 DO  (*pointer base fixup*)
        # #     Texts.WriteString(W, " ->"); Texts.WriteInt(W, class, 4); Read(R, class)
        # #   END ;
        # #   Texts.Write(W, ")")
        if klass == symbols.Typ:
            t += '('
            while True:  # pointer base fixup
                klass = f.read()
                if klass == 0:
                    break
                t += ' -> %4d' % (klass,)
            print t+')'
        # # ELSIF (class = ORB.Const) OR (class = ORB.Var) THEN
        # #   Files.ReadNum(R, k); Texts.WriteInt(W, k, 5);  (*Reals, Strings!*)
        # # END ;
        elif klass in (symbols.Const, symbols.Var):
            print '%s %5d' % (t,f.read_num())
        # # Texts.WriteLn(W); Texts.Append(Oberon.Log, W.buf);
        # # Read(R, class)
        # # END (* while loop *)

def reg_name(r):
    if r < 12:
        t = '  R%d ' % (r & 0xf)
    elif r = 12:
        t = ' MT'
    elif r = 13:
        t = ' SB'
    elif r = 14:
        t = ' SP'
    else:
        t = ' LNK'
    print t,

mnemo0 = ['MOV', 'LSL', 'ASR', 'ROR', 'AND', 'ANN', 'IOR', 'XOR',
          'ADD', 'SUB', 'MUL', 'DIV', 'FAD', 'FSB', 'FML', 'FDV']

mnemo1 = ['MI ', 'EQ ', 'LS ', '???', '???', 'LT ', 'LE ', '???',
          'PL ', 'NE ', 'HI ', '???', '???', 'GE ', 'GT ', 'NO ']
 
def opcode(w):
    k = (w >> 30) & 0x3  # first two bits: Format type
    a = (w >> 24) & 0xf  # second nybble: Reg A
    b = (w >> 20) & 0xf  # third nybble: Reg B
    op = (w >> 16) & 0xf  # fourth nybble: opcode (for Format type 0 and 1)
    u = (w >> 28) & 0x1  # u bit in first nybble
    t = ''
    if k == 0:  # Format type 0
        t += mnemo0[op]
        if u == 1:
            t += "'"
        t += reg_name(a)
        t += reg_name(b)
        t += reg_name(w & 0xf)  # last nybble: Reg C
    elif k == 1:  # Format type 1
        t += mnemo0[op]
        if u == 1:
            t += "'"
        t += reg_name(a)
        t += reg_name(b)
        w &= 0xffff
        if w >= 0x8000:
            w -= 0x10000
        t += ('%7d' % (w,))
    elif k == 2:  # Format type 2
        # LDR/STR
        if u == 1:
            t += 'STR '
        else:
            t += 'LDR'
        t += reg_name(a)
        t += reg_name(b)
        w &= 0xfffff
        if w >= 0x80000:
            w -= 0x100000
        t += ('%8d' % (w,))
    elif k == 3:  # Format type 3
        # Branch instruction
        t += 'B'
        if w & 0x10000000:  # check v bit
            t += 'L'
        t += mnemo1[a]
        if u == 0:
            t += reg_name(w & 0xf)
        else:
            # note: the original may have an error...
            # I deliberately copy that error :-)
            w = w & 0xfffff;
            if w >= 0x80000:
                w -= 0x100000
            t += ('%8d' % (w,))
    return t

def sync(f):
    ch = f.read(1)
    print "Sync "+ch

def decode_object(fname):
    # decode object file
    print 'Decode', fname
    f = symbols.SerializingReader(fname)
    if f.content == None:
        f.close()
        print '%s not found' % (fname,)
        return
    print
    # # Files.Set(R, F, 0); Files.ReadString(R, name); Texts.WriteLn(W); Texts.WriteString(W, name);
    # # Files.ReadInt(R, key); Texts.WriteHex(W, key); Read(R, class); Texts.WriteInt(W, class, 4); (*version*)
    # # Files.ReadInt(R, size); Texts.WriteInt(W, size, 6); Texts.WriteLn(W);
    name = f.read_string()
    key = f.read_int()
    klass = f.read()  # version
    size = f.read_int()
    print '%s %08X %4d %6d' % (name,key,klass,size)
    # # Texts.WriteString(W, "imports:"); Texts.WriteLn(W); Files.ReadString(R, name);
    # # WHILE name[0] # 0X DO
    # #   Texts.Write(W, 9X); Texts.WriteString(W, name);
    # #   Files.ReadInt(R, key); Texts.WriteHex(W, key); Texts.WriteLn(W);
    # #   Files.ReadString(R, name)
    # # END ;
    # # (* Sync(R); *)
    print 'imports:'
    while True:
        name = f.read_string()
        if name == '': # CHECKTHAT
            break
        key = f.read_int()
        print '\t%s %08X' % (name,key)
    # sync(f)
    # # Texts.WriteString(W, "type descriptors"); Texts.WriteLn(W);
    # # Files.ReadInt(R, n); n := n DIV 4; i := 0;
    # # WHILE i < n DO Files.ReadInt(R, data); Texts.WriteHex(W, data); INC(i) END ;
    # # Texts.WriteLn(W);
    print 'type descriptors'
    n = f.read_int()/4
    t = ''
    for i in range(n):
        t += ' %08X' % (f.read_int(),)
    print t
    # # Texts.WriteString(W, "data"); Files.ReadInt(R, data); Texts.WriteInt(W, data, 6); Texts.WriteLn(W);
    print 'data %6d' % (f.read_int(),)
    # # Texts.WriteString(W, "strings"); Texts.WriteLn(W);
    # # Files.ReadInt(R, n); i := 0;
    # # WHILE i < n DO Files.Read(R, ch); Texts.Write(W, ch); INC(i) END ;
    # # Texts.WriteLn(W);
    print 'strings'
    n = f.read_int()
    t = ''
    for i in range(n):
        t += '%c' % (f.read(),)
    print t
    # # Texts.WriteString(W, "code"); Texts.WriteLn(W);
    # # Files.ReadInt(R, n); i := 0;
    # # WHILE i < n DO
    # #   Files.ReadInt(R, data); Texts.WriteInt(W, i, 4); Texts.Write(W, 9X); Texts.WriteHex(W, data);
    # #   Texts.Write(W, 9X); opcode(data); Texts.WriteLn(W); INC(i)
    # # END ;
    # # (* Sync(R); *)
    print 'code'
    n = f.read_int()
    for i in range(n):
        data = f.read_int()
        print ' %4d\t %08X\t%s' % (i,data,opcode(data))
    # sync(f)
    # # Texts.WriteString(W, "commands:"); Texts.WriteLn(W);
    # # Files.ReadString(R, name);
    # # WHILE name[0] # 0X DO
    # #   Texts.Write(W, 9X); Texts.WriteString(W, name);
    # #   Files.ReadInt(R, adr); Texts.WriteInt(W, adr, 5); Texts.WriteLn(W);
    # #   Files.ReadString(R, name)
    # # END ;
    # # (* Sync(R); *)
    print 'commands:'
    while True:
        name = f.read_string()
        if name == '':  # CHECKTHAT
            break
        addr = f.read_int()
        print '\t%s %5d' % (name,addr)
    # sync(f)
    # # Texts.WriteString(W, "entries"); Texts.WriteLn(W);
    # # Files.ReadInt(R, n); i := 0;
    # # WHILE i < n DO
    # #   Files.ReadInt(R, adr); Texts.WriteInt(W, adr, 6); INC(i)
    # # END ;
    # # Texts.WriteLn(W);
    # # (* Sync(R); *)
    print 'entries'
    n = f.read_int()
    t = ''
    for i in range(n):
        t += ' %6d' % (f.read_int(),)  # read in and write out addr
    print t
    # sync(f)
    # # Texts.WriteString(W, "pointer refs"); Texts.WriteLn(W); Files.ReadInt(R, adr);
    # # WHILE adr # -1 DO Texts.WriteInt(W, adr, 6); Files.ReadInt(R, adr) END ;
    # # Texts.WriteLn(W);
    # # (* Sync(R); *)
    print 'pointer refs'
    t = ''
    while True:
        addr = f.read_int()
        if addr == -1:
            break
        t += ' %6d' % (addr,)
    print t
    # sync(f)
    # # Files.ReadInt(R, data); Texts.WriteString(W, "fixP = "); Texts.WriteInt(W, data, 8); Texts.WriteLn(W);
    # # Files.ReadInt(R, data); Texts.WriteString(W, "fixD = "); Texts.WriteInt(W, data, 8); Texts.WriteLn(W);
    # # Files.ReadInt(R, data); Texts.WriteString(W, "fixT = "); Texts.WriteInt(W, data, 8); Texts.WriteLn(W);
    # # Files.ReadInt(R, data); Texts.WriteString(W, "entry = "); Texts.WriteInt(W, data, 8); Texts.WriteLn(W);
    # # Files.Read(R, ch);
    # # IF ch # "O" THEN Texts.WriteString(W, "format eror"); Texts.WriteLn(W) END
    # # (* Sync(R); *)
    print 'fixP =  %8d' % (f.read_int(),)
    print 'fixD =  %8d' % (f.read_int(),)
    print 'fixI =  %8d' % (f.read_int(),)
    print 'entry =  %8d' % (f.read_int(),)
    ch = f.read()  # CHECKTHAT
    if ch != 'O':
        print 'format eror'
    # sync(f)
    f.close()
