# PyOberon - A Python-based Oberon 07 Compiler

This project aims to reproduce the original Oberon 07 compiler (naturally written in itself) using Python and PLY (taken from https://github.com/dabeaz/ply). It aims to be able to do the following:

* Be able to produce an object file just (hopefully) exactly the same as the original Oberon compiler (I am a bit concerned regarding the possibility of TRAP instructions having different line numbers due to the nature of PLY tools used...for now I shall just plant "line 1"...I'm damn lazy :-))
* Be able to read in symbol files that would be produced by the original Oberon compiler
* Be able to create symbol files that the original Oberon compiler would understand

I also have my personal goals:

* I want to understand how record type checking works for the purpose of object-oriented programming
* I want to understand how the exported symbols get read in and written out to
* I want the compiler to be reasonably modular, so I can rip parts of my compiler and use it for other projects
* I want it to be reasonably flexible, to allow for rapid evolution of language should one decide to fork this project

But I don't want it to get too baroque...just keep it simple and clean. Just clone the functionality, and that's about it. But I do implement things a bit differently (like using pile instead of purely stack based information processing).

Oh, one other thing: PyOberon is currently written in Python 2.7. But someday it will also support Python 3.x. Right now: get it working first!!! :-)

## Installing and Testing

Create a directory and place all files in that directory. Download and install PLY from https://github.com/dabeaz/ply website. Then modify the sys.path line near the beginning in the scanner.py, parser.py and tester.py to point to the PLY directory. The rest will be written later.

## Usage

You can try the parser right now by typing `python tester.py {Oberon source file}`. You may see those warnings:

```
    WARNING: No ignore rule is defined for exclusive state 'comment'
    WARNING: no p_error() function is defined
```

Just ignore them. It will be OK. When the parser completes its job, you can view the `testout.txt` file to see the AST representation.

## Progress status

- [x] `scanner.py` - A lexical scanner using `lex.py`. Looks pretty done, though there *might* be pathological case involving real numbers. I leave that to others to verify that! \[evil grin\]
- [x] `oberonparser.py` - A syntactical parser using `yacc.py`. Looks quite done, and there are a few comments pointing out parsing issues that the analyzer needs to do, and any deviations and undocumented grammar as well. Note that this file needs to be called `oberonparser.py` instead of `parser.py`, since Python has a built-in module called `parser`, which is **NOT** what we want!!! :-)
- [x] `pile.py` - A lightweight semi-journaling stack-like data structure thingie. It has a testing suite of sort that you can play with. I would say it is done. :-)
- [ ] `symbols.py` - Maintains symbol table, plus routines for reading and writing symbol files. It is not really finished.
- [ ] `analyzer.py` - This module fixes up any issues that the parser cannot do (like declarators), read in symbol files, and build its own symbol table using pile. Being developed.
- [ ] `optimizer.py` - A machine-independent optimizations as done by the original Oberon compiler. It includes constant folding and conversion of multiply/divide by powers of 2 to shifts, and more. Yet to be created.
- [ ] `codegenr.py` - A code generator targeting the original RISC 5 platform. Translated in a nearly naive fashion; untested.
- [ ] `tools.py` - A clone of `ORTools.Mod.Txt`. I realize I would need that to help decode the logic, and to enable some verifications of both symbol files and object files. I believe it is pretty much done, but it is totally untested right now.
- [ ] `compiler.py` - Right now that role is filled by `tester.py`, and that is obviously constantly evolving. One day it will tie up all parts together and have PyOberon running like a charm.

## Contributing

Will be written later.

### Branching

Will be written later.

### Development rules

Although I have yet to fully flesh it out for the purpose of collaboration, I need to mention a few rules right away to keep it at least slightly sane:

1. **NO TABS** for indentation. Use spaces **ONLY**!!! And each indentation should be 4 spaces.
2. Certain Python files will have the original Oberon code in comments, to aid in translation. Those source lines are rather special, and so shall follow the following rules:
   2a. Each such line shall be marked with \# \# (two hashes..these are easily generated in Notepad++ using `ctrl-K`)
   2b. These lines are to be indented independently of the actual Python code so that if one remove all Python codes and standard comments (those starting with a single hash), they would be properly indented. Note that unlike the original Oberon code, all these indentations are made using 4 spaces rule.
3. Although I know I don't always follow that rule here (yet), but each part (like classes, functions, whatever), if complicated enough, should be annotated.
4. The coding should be clear. Cutesy early 80s short spelling such as `mno' are to be strongly discouraged!!!! They make code reading harder to figure out. Of course everything is in flux as I am continually translating the code, so I will be tolerant of that for a while...

Note that rule 2b is possible thanks to Python's liberal acceptance of comments being placed regardless of indentations (thanks Guido! :-))

## Bugs

Obviously a lot, as it is not even in alpha stage (whatever that means). But the scanner/parser modules should work though. But I think a separate list of bugs/mistakes need to be made regarding the original Oberon compiler, just to keep things clear. That is not even written up yet!

## Legal

The source for PyOberon is released under the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
