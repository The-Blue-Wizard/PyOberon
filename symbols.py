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


# symbols

import struct
import pile

symbol_table = pile.Pile()

versionkey = 1

# class numbering as used in ORB.Mod

Head  = 0
Const = 1
Var   = 2
Par   = 3
Fld   = 4
Typ   = 5
SProc = 6
SFunc = 7
Mod   = 8

# form values as used in ORB.Mod

Byte = 1
Bool = 2
Char = 3
Int  = 4
Real = 5
Set  = 6
Pointer = 7
NilTyp  = 8
NoTyp   = 9
Proc    = 10
String  = 11
Array   = 12
Record  = 13

class Struct:
    # Dummy class, used as records
    pass

# from ORB.Mod:
# # ObjDesc*= RECORD
  # # class*, lev*, exno*: INTEGER;
  # # expo*, rdo*: BOOLEAN;   (*exported / read-only*)
  # # next*, dsc*: Object;
  # # type*: Type;
  # # name*: ORS.Ident;
  # # val*: LONGINT
# # END ;
class Object:
    def __init__(self, name, klass):
        self.name = name
        self.klass = klass
        self.exported = False
        self.read_only = False

# from ORB.Mod:
# # ModDesc* = RECORD (ObjDesc) orgname*: ORS.Ident END ;
class Module(Object):
    def __init__(self, name, orgname, key):
        self.__init__(name, Mod)
        self.orgname = orgname
        self.val = key
        self.type = noTyp

# from ORB.Mod:
# # TypeDesc* = RECORD
  # # form*, ref*, mno*: INTEGER;  (*ref is only used for import/export*)
  # # nofpar*: INTEGER;  (*for procedures, extension level for records*)
  # # len*: LONGINT;  (*for arrays, len < 0 => open array; for records: adr of descriptor*)
  # # dsc*, typobj*: Object;
  # # base*: Type;  (*for arrays, records, pointers*)
  # # size*: LONGINT;  (*in bytes; always multiple of 4, except for Byte, Bool and Char*)
# # END ;
class Type:
    def __init__(self):
        self.form = NoTyp
        self.base = None

class SerializingReader:
    # This is an Oberon style deserializer helper class
    # most of them are originally in Files.Mod
    def __init__(self, fname):
        self.pos = 0
        f = open(fname, 'rb')
        if f == None:
            self.content = None
            return
        self.content = f.read()
        f.close()

    # # PROCEDURE Files.ReadByte*(VAR r: Rider; VAR x: BYTE);
    def read_byte(self):
        k = self.pos
        if k >= len(self.content):
            return None
        ch = ord(self.content[k])
        self.pos = k+1
        return ch

    # # PROCEDURE ORB.Read(VAR R: Files.Rider; VAR x: INTEGER);
    def read(self):  # "moved" from ORTool
        # # VAR b: BYTE;
        # # BEGIN Files.ReadByte(R, b);
        # # IF b < 80H THEN x := b ELSE x := b - 100H END
        n = self.read_byte()
        if n < 0x80:
            return n
        return n - 0x100
    # # END Read;

    # # PROCEDURE Files.ReadNum*(VAR R: Rider; VAR x: INTEGER);
    def read_num(self):  # FIXME
        # # VAR n, y: INTEGER; b: BYTE;
        # BEGIN n := 32; y := 0; ReadByte(R, b);
        n = 32
        y = 0
        # WHILE b >= 80H DO y := ROR(y + b-80H, 7); DEC(n, 7); ReadByte(R, b) END ;
        k = 0
        while True:
            b = self.read_byte()
            if b < 0x80:
                break
            y += (1<<k)*(b-0x80)
            k += 7
            n -= 7
        y += (1<<k)*(b & 0x3f)
        # IF n <= 4 THEN x := ROR(y + b MOD 10H, 4) ELSE x := ASR(ROR(y + b, 7), n-7) END
        if n <= 4:
            # b must be in range 0 to 15
            if b >= 8:
            # equivalently, if y > 2**31-1:
                y -= 2**32
            return y
        if b >= 0x40:
             y -= 2**(k+6)
        return y
    # # END ReadNum;

    # # PROCEDURE Files.ReadInt*(VAR R: Rider; VAR x: INTEGER);
    def read_int(self):
        k = self.pos
        if k >= len(self.content)+3:
            return None
        n = struct.pack('>i',self.content[k:k+4])
        self.pos = k+4
        return n
    # # END ReadInt;

    # # PROCEDURE Files.ReadString*(VAR R: Rider; VAR x: ARRAY OF CHAR);
    def read_string(self):
        oldpos = self.pos
        k = self.content.find('\0', oldpos)
        self.pos = k+1
        return self.content[oldpos:k]
    # # END ReadString;

    def close(self):
        self.content = None # empty it out

# # PROCEDURE ThisModule(name, orgname: ORS.Ident; non: BOOLEAN; key: LONGINT): Object;
def find_module(name, orgname, non, key):
    global nofmod
    # # VAR mod: Module; obj, obj1: Object;
    # # BEGIN obj1 := topScope; obj := obj1.next;  (*search for module*)
    # obj1 = topScope
    # obj = obj1.next  # search for module
    # # WHILE (obj # NIL) & (obj.name # name) DO obj1 := obj; obj := obj1.next END ;
    # while (obj != None) and (obj.name != name):
        # obj1 = obj
        # obj = obj1.next
    objref = symbol_table.find(name) # FIXME
    # # IF obj = NIL THEN  (*insert new module*)
    # if obj == None:  # insert new module
    if objref[0] == None:  # insert new module
        # # NEW(mod); mod.class := Mod; mod.rdo := FALSE;
        # # mod.name := name; mod.orgname := orgname; mod.val := key;
        mod = Module(name, orgname, key)
        # # mod.lev := nofmod; INC(nofmod); mod.type := noType; mod.dsc := NIL; mod.next := NIL;
        mod.lev = nofmod
        nofmod += 1
        mod.dsc = None
        mod.next = None
        # # obj1.next := mod; obj := mod
        # obj1.next = mod
        # obj = mod
        symbol_table.add(mod)
    # # ELSE (*module already present*)
    else:  # module already present
        # # IF non THEN ORS.Mark("invalid import order") END
        if non:
            print "invalid import order"
    # # END ;
    # # RETURN obj
    return obj
# # END ThisModule;

# # PROCEDURE InType(VAR R: Files.Rider; thismod: Object; VAR T: Type);
def read_type(r, thismod):
    # # VAR key: LONGINT;
    # # ref, class, mno, form, np, readonly: INTEGER;
    # # new, fld, par, obj, mod, impmod: Object;
    # # t: Type;
    # # name, modname: ORS.Ident;
    # BEGIN Read(R, ref);
    ref = r.read()
    # IF ref < 0 THEN T := typtab[-ref]  (*already read*)
    if ref < 0:
        return typtab[-ref]  # already read
    # ELSE NEW(t); T := t; typtab[ref] := t; t.mno := thismod.lev;
    t = Type()
    T = t
    typtab[ref] = t
    t.mno = thismod.lev;
        # Read(R, form); t.form := form;
    form = r.read()
    t.form = form
        # IF form = Pointer THEN InType(R, thismod, t.base); t.size := 4
    if form == Pointer:
        t.base = read_type(r, thismod)
        t.size = 4
        # ELSIF form = Array THEN
    elif form == Array:
            # InType(R, thismod, t.base); Files.ReadNum(R, t.len); Files.ReadNum(R, t.size)
        t.base = read_type(r, thismod)
        t.len = r.read_num()
        t.size = r.read_num()
        # ELSIF form = Record THEN
    elif form == Record:
            # InType(R, thismod, t.base);
        t.base = read_type(r, thismod)
            # IF t.base.form = NoTyp THEN t.base := NIL; obj := NIL ELSE obj := t.base.dsc END ;
        if t.base.form == NoTyp:
            t.base = None
            obj = None
        else:
            obj = t.base.dsc
            # Files.ReadNum(R, t.len); (*TD adr/exno*)
        t.len = r.read_num()  # TD adr/exno
            # Files.ReadNum(R, t.nofpar);  (*ext level*)
        t.nofpar = r.read_num()  # ext level
            # Files.ReadNum(R, t.size);
        t.size = r.read_num()
            # Read(R, class);
        klass = r.read()
            # WHILE class # 0 DO  (*fields*)
        while klass != 0:  # fields
                # NEW(fld); fld.class := class; Files.ReadString(R, fld.name);
            fld = Struct()
            fld.klass = klass
            fld.name = r. read_string()
                # IF fld.name[0] # 0X THEN fld.expo := TRUE; InType(R, thismod, fld.type) ELSE fld.expo := FALSE; fld.type := nilType END ;
            if fld.name != '':
                fld.expo = True
                fld.type = read_type(r, thismod)
                fld.expo = False
                fld.type = nilType
                # Files.ReadNum(R, fld.val); fld.next := obj; obj := fld; Read(R, class)
            fld.val = read_type(r, thismod)
            fld.next = obj
            obj = fld
            klass = r.read()
            # END ;
            # t.dsc := obj
        t.dsc = obj
        # ELSIF form = Proc THEN
    elif form == Proc:
            # InType(R, thismod, t.base);
        t.base = read_type(r, thismod)
            # obj := NIL; np := 0; Read(R, class);
        obj = None
        np = 0
        klass = r.read()
            # WHILE class # 0 DO  (*parameters*)
        while klass != 0:  # parameters
                # NEW(par); par.class := class; Read(R, readonly); par.rdo := readonly = 1; 
            par = Object(None, klass)
            readonly = r.read()
            par.rdo = (readonly == 1)
                # InType(R, thismod, par.type); par.next := obj; obj := par; INC(np); Read(R, class)
            par.type = read_type(r, thismod)
            par.next = obj
            obj = par
            np += 1
            klass = r.read()
            # END ;
            # t.dsc := obj; t.nofpar := np; t.size := 4
        t.dsc = obj
        t.nofpar = np
        t.size = 4
        # END ;
        # Files.ReadString(R, modname);
    modname = r.read_string()
        # IF modname[0] #  0X THEN  (*re-import*)
    if modname != '':  # re-import
            # Files.ReadInt(R, key); Files.ReadString(R, name);
        key = r.read_int()
        name = r. read_string()
            # mod := ThisModule(modname, modname, FALSE, key);
        mod = ThisModule(modname, modname, FALSE, key)  # FIXME
            # obj := mod.dsc;  (*search type*)
        obj = mod.dsc  # search type
            # WHILE (obj # NIL) & (obj.name # name) DO obj := obj.next END ;
        while (obj != None) and (obj.name != name):
            obj = obj.next
            # IF obj # NIL THEN T := obj.type   (*type object found in object list of mod*)
        if obj != None:
            T = obj.type  # type object found in object list of mod
            # ELSE (*insert new type object in object list of mod*)
        else:  # insert new type object in object list of mod
                # NEW(obj); obj.name := name; obj.class := Typ; obj.next := mod.dsc; mod.dsc := obj; obj.type := t;
            obj = Struct()
            obj.name = name
            obj.klass = Typ
            obj.next = mod.dsc
            mod.dsc = obj
            obj.type = t
                # t.mno := mod.lev; t.typobj := obj; T := t
            t.mno = mod.lev
            t.typobj = obj
            T = t
            # END ;
            # typtab[ref] := T
        typtab[ref] = T
        # END
    # END
    return T

# # PROCEDURE Import*(VAR modid, modid1: ORS.Ident);
def read_symbols(import_alias, module_name):
    # # VAR key: LONGINT; class, k: INTEGER;
      # # obj: Object;  t: Type;
      # # thismod: Object;
      # # modname, fname: ORS.Ident;
      # # F: Files.File; R: Files.Rider;
    # # BEGIN
    # # IF modid1 = "SYSTEM" THEN
    if module_name == 'SYSTEM':
        # # thismod := ThisModule(modid, modid1, TRUE, key); DEC(nofmod);
        # # thismod.lev := 0; thismod.dsc := system; thismod.rdo := TRUE
        import_list.append('SYSTEM')
        return 'junk' # FIXME
    # # ELSE MakeFileName(fname, modid1, ".smb"); F := Files.Old(fname);
    r = SerializingReader(module_name + '.smb')
    # # IF F # NIL THEN
    if r.content == None:
        error('Import file "%s" not found' % (import_name,))
        return
        # # Files.Set(R, F, 0); Files.ReadInt(R, key); Files.ReadInt(R, key); Files.ReadString(R, modname);
        # # thismod := ThisModule(modid, modid1, TRUE, key); thismod.rdo := TRUE;
        # # Read(R, class); (*version key*)
    # skip first 2 integers, then read in...
    k = r.read_int()
    k = r.read_int()
    file_module_name = r.read_string()
        # # IF class # versionkey THEN ORS.Mark("wrong version") END ;
    ver = r.read_byte()
    if ver != versionkey:
        error('Wrong version')
        return
        # # Read(R, class);
        # # WHILE class # 0 DO
    while True:
        klass = r.read()
        if klass == 0:
            break
            # # NEW(obj); obj.class := class; Files.ReadString(R, obj.name);
        obj = Object(r.read_string(), klass)
            # # InType(R, thismod, obj.type); obj.lev := -thismod.lev;
        obj.type = read_type(r)
        obj.lev = r.read_info()
            # # IF class = Typ THEN
        if klass == Typ:
                # # t := obj.type; t.typobj := obj; Read(R, k);  (*fixup bases of previously declared pointer types*)
            t = obj.type
            t.typobj = obj
            # # WHILE k # 0 DO typtab[k].base := t; Read(R, k) END
            while True:
                k = f.read()
                if k == 0:
                    break
                typtab[k].base = t
            # # ELSE
        else:
                # # IF class = Const THEN
            if klass == Const:
                    # # IF obj.type.form = Real THEN Files.ReadInt(R, obj.val) ELSE Files.ReadNum(R, obj.val) END
                if obj.type.form == Real:
                    obj.val = f.read_int()
                else:
                    obj.val = f.read_num()
                # # ELSIF class = Var THEN Files.ReadNum(R, obj.val); obj.rdo := TRUE
            elif klass == Var:
                obj.val = f.read_num()
                obj.read_only = True
                # # END
            # # END ;
            # # obj.next := thismod.dsc; thismod.dsc := obj; Read(R, class)
        thismod.append(obj)

class SerializingWriter:
    # This is an Oberon style serializer helper class
    def __init__(self, fname):
        self.f = open(fname, 'wb')

    # # PROCEDURE Files.WriteByte*(VAR r: Rider; x: BYTE);
    def write_byte(self, n):
        self.f.write(chr(n))
    # # END WriteByte;

    # # PROCEDURE Files.Write*(VAR r: Rider; ch: CHAR);
    def write(self, n):
        if n < 0:
            n += 0x80
        self.write_byte(n)
    # # END Write;

    # # PROCEDURE Files.WriteInt*(VAR R: Rider; x: INTEGER);
    def write_int(self, n):
        pass  # FIXME
    # # END WriteInt;

    # # PROCEDURE Files.WriteString*(VAR R: Rider; x: ARRAY OF CHAR);
    def write_string(self, s):
        self.f.write(s+'\0')
    # # END WriteString;

    # # PROCEDURE Files.WriteNum*(VAR R: Rider; x: INTEGER);
    def write_num(self, n):
        # # BEGIN
        # # WHILE (x < -40H) OR (x >= 40H) DO WriteByte(R, x MOD 80H + 80H); x := ASR(x, 7) END ;
        while (n < -0x40) or (n >= 0x40):
            self.f.write((n & 0x7F) + 0x80)
            n >>= 7  # note it is sign-preserved!
        # # WriteByte(R, x MOD 80H)
        self.f.write(n & 0x7F)
    # # END WriteNum;

    def find_hidden_pointers(self, typ, offset):
        if typ.form in (Pointer, NilTyp):
            self.write(Fld)
            self.write(0)
            self.write_num(offset)
        elif typ.form == Record:
            for fld in typ.dsc:
                self.find_hidden_pointers(fld.typ, fld.var+offset)
        elif typ.form == Array:
            for i in range(typ.len):
                self.find_hidden_pointers(typ.base, typ.base.size*i+offset)
    def close(self):
        self.f.close()

# # PROCEDURE OutType(VAR R: Files.Rider; t: Type);
    # # VAR obj, mod, fld: Object;
def write_type(w, t):
    global Ref
# # BEGIN
    # # IF t.ref > 0 THEN (*type was already output*) Write(R, -t.ref)
    if t.ref > 0:
        # type was already written to
        w.write(-t.ref)
        return
    # # ELSE obj := t.typobj;
    obj = t.typobj
        # # IF obj # NIL THEN Write(R, Ref); t.ref := Ref; INC(Ref) ELSE (*anonymous*) Write(R, 0) END ;
    if obj != None:
        w.write(Ref)
        t.ref = Ref
        Ref += 1
    else:
        # anonymous
        w.write(0)
        # # Write(R, t.form);
    w.write(t.form)
        # # IF t.form = Pointer THEN OutType(R, t.base)
    if t.form == Pointer:
        write_type(w, t.base)
        # # ELSIF t.form = Array THEN OutType(R, t.base); Files.WriteNum(R, t.len); Files.WriteNum(R, t.size)
    elif t.form == Array:
        write_type(w, t.base)
        w.write_int(t.len)
        w.write_int(t.size)
        # # ELSIF t.form = Record THEN
    elif t.form == Record:
            # # IF t.base # NIL THEN OutType(R, t.base) ELSE OutType(R, noType) END ;
        if t.base != None:
            write_type(w, t.base)
        else:
            write_type(w, noType)
            # # IF obj # NIL THEN Files.WriteNum(R, obj.exno) ELSE Write(R, 0) END ;
        if obj != None:
            w.write_num(obj.exno)
        else:
            w.write(0)
            # # Files.WriteNum(R, t.nofpar); Files.WriteNum(R, t.size);
        f.write_num(t.nofpar)
        f.write_num(t.size)
            # # fld := t.dsc;
            # # WHILE fld # NIL DO  (*fields*)
        for fld in t.dsc:
            # fields
                # # IF fld.expo THEN
            if fld.exported:
                    # # Write(R, Fld); Files.WriteString(R, fld.name); OutType(R, fld.type); Files.WriteNum(R, fld.val)
                w.write(Fld)
                w.write_string(fld.name)
                write_type(w, fld.type)
                w.write_num(fld.val)
                # # ELSE FindHiddenPointers(R, fld.type, fld.val)  (*offset*)
            else:
                w.find_hidden_pointers(fld.type, fld.val)  # offset
                # # END ;
                # # fld := fld.next
            # # END ;
            # # Write(R, 0)
        w.write(0)
        # # ELSIF t.form = Proc THEN OutType(R, t.base); OutPar(R, t.dsc, t.nofpar); Write(R, 0)
    elif t.form == Proc:
        write_type(w, t.base)
        write_par(w, t.dsc, t.nofpar)
        w.write(0)
        # # END ;
        # # IF (t.mno > 0) & (obj # NIL) THEN  (*re-export, output name*)
    if t.mno > 0 and obj != None:
        # re-export, output name
          # # mod := topScope.next;
          # # WHILE (mod # NIL) & (mod.lev # t.mno) DO mod := mod.next END ;
          # # IF mod # NIL THEN Files.WriteString(R, mod.name); Files.WriteInt(R, mod.val); Files.WriteString(R, obj.name)
          # # ELSE ORS.Mark("re-export not found"); Write(R, 0)
          # # END
        # # ELSE Write(R, 0)
        # # END
        # if mod is found with t.mno:
        if mod:  # FIXME
            w.write_string(mod.name)
            w.write_int(mod.val)
            w.write_string(obj.name)
        else:
            error("re-export not found")
            w.write(0)
    else:
        w.write(0)
    # # END
# # END OutType;

# # PROCEDURE Export*(VAR modid: ORS.Ident; VAR newSF: BOOLEAN; VAR key: LONGINT);
def write_symbols(module_name):
    # # VAR x, sum, oldkey: LONGINT;
      # # obj, obj0: Object;
      # # filename: ORS.Ident;
      # # F, F1: Files.File; R, R1: Files.Rider;
    # # BEGIN Ref := Record + 1; MakeFileName(filename, modid, ".smb");
    # # F := Files.New(filename); Files.Set(R, F, 0);
    w = SerializingWriter(module_name + '.smb')
    # # Files.WriteInt(R, 0); (*placeholder*)
    w.write_int(0)  # placeholder
    # # Files.WriteInt(R, 0); (*placeholder for key to be inserted at the end*)
    w.write_int(0)  # placeholder for key to be inserted at the end
    # # Files.WriteString(R, modid); Write(R, versionkey);
    w.write_string(module_name)
    w.write(versionkey)
    # # obj := topScope.next;
    # # WHILE obj # NIL DO
    while obj in obj_list:
        # # IF obj.expo THEN
        # #     ...
        # # END ;
        # # obj := obj.next
        if not obj.exported:
            continue
        # # Write(R, obj.class); Files.WriteString(R, obj.name);
        w.write(obj.klass)
        w.write_string(obj.name)
        # # OutType(R, obj.type);
        write_type(w, obj.type)
        # # IF obj.class = Typ THEN
        if obj.classnum == Typ:
            # # IF obj.type.form = Record THEN
            if obj.type.form == Record:
                # # obj0 := topScope.next;  (*check whether this is base of previously declared pointer types*)
                # # WHILE obj0 # obj DO
                while obj0 in something:
                    # # IF (obj0.type.form = Pointer) & (obj0.type.base = obj.type) & (obj0.type.ref > 0) THEN Write(R, obj0.type.ref) END ;
                    # # obj0 := obj0.next
                    if (obj0.type.form == Pointer) and (obj0.type.base == obj.type) and (obj0.type.ref > 0):
                        w.write(obj0.type.ref)
                # # END
            # # END ;
            # # Write(R, 0)
            w.write(0)
        # # ELSIF obj.class = Const THEN
        elif obj.classnum == Const:
            # # IF obj.type.form = Proc THEN Files.WriteNum(R, obj.exno)
            if obj.type.form == Proc:
                w.write_num(obj.exno)
            # # ELSIF obj.type.form = Real THEN Files.WriteInt(R, obj.val)
            elif obj.type.form == Real:
                w.write_int(obj.val)
            # # ELSE Files.WriteNum(R, obj.val)
            else:
                w.write_num(obj.val)
            # # END
        # # ELSIF obj.class = Var THEN
        elif obj.classnum == Var:
            # # Files.WriteNum(R, obj.exno);
            w.write_num(obj.exno)
            # # IF obj.type.form = String THEN
            if obj.type.form == String:
                # # Files.WriteNum(R, obj.val DIV 10000H); obj.val := obj.val MOD 10000H
                w.write_num(obj.val / 0x10000)
                obj.val %= 0x10000
            # # END
        # # END
    # # END ;
    # # REPEAT Write(R, 0) UNTIL Files.Length(F) MOD 4 = 0;
    while True:
        w.write_byte(0)
        if f.size() % 4 == 0:
            break
    # # Files.Set(R, F, 0); sum := 0; Files.ReadInt(R, x);  (* compute key (checksum) *)
    w.close()
    # some extra stuff to do here?
    # FIXME
    # # FOR Ref := Record+1 TO maxTypTab-1 DO typtab[Ref] := NIL END ;
    r = SymbolReader(module_name + '.smb')
    # # WHILE ~R.eof DO sum := sum + x; Files.ReadInt(R, x) END ;
    # # F1 := Files.Old(filename); (*sum is new key*)
    # # IF F1 # NIL THEN Files.Set(R1, F1, 4); Files.ReadInt(R1, oldkey) ELSE oldkey := sum+1 END ;
    # # IF sum # oldkey THEN
        # # IF newSF OR (F1 = NIL) THEN
            # # key := sum; newSF := TRUE; Files.Set(R, F, 4); Files.WriteInt(R, sum); Files.Register(F)  (*insert checksum*)
        # # ELSE ORS.Mark("new symbol file inhibited")
        # # END
    # # ELSE newSF := FALSE; key := sum
    # # END
    checksum = 0
    n = 0
    while n != None:
        checksum += n
        n = r.read_int()
    r.close()
    # more code to do here
# # END Export;
