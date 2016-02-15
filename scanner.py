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


# Translated from ORS.Mod (NW 19.9.93 / 1.4.2014  Scanner in Oberon-07)

# This version of Oberon Scanner does lexical analysis, with a few differences.
# The input is ANSI text (not Oberon-Text), so as to make it more portable. The
# output is a sequence of symbols, i.e identifiers, numbers, strings, and special
# symbols.

# It recognizes all Oberon keywords and skips comments. The keywords are
# recorded in a tuple.

import sys
sys.path.insert(0,r"C:\Users\user1\wbench\ply-3.8")
from ply import *
import struct

keywords = ["IF", "DO", "OF", "OR", "TO", "IN", "IS", "BY", "END",
    "NIL", "VAR", "DIV", "MOD", "FOR", "ELSE", "THEN", "TRUE",
    "TYPE", "CASE", "ELSIF", "FALSE", "ARRAY", "BEGIN", "CONST",
    "UNTIL", "WHILE", "RECORD", "REPEAT", "RETURN", "IMPORT",
    "MODULE", "POINTER", "PROCEDURE"]

# By default the scanner ignores comments. Call ignore_comments()
# to get or ignore comments. Incidentially, it also configures
# the tokens list.

get_comments = False

# List of nonkeyword token names.
other_tokens = [
    'STRING',   'NE',      'AND',     'LPAREN', 'RPAREN',
    'STAR',     'PLUS',    'COMMA',   'MINUS',  'UPTO',
    'DOT',      'BECOMES', 'COLON',   'SEMI',   'LE',
    'LT',       'EQ',      'GE',      'GT',     'LBRACE',
    'BAR',      'RBRACE',  'TILDE',   'LBRAK',  'RBRAK',
    'ARROW',    'INTEGER', 'REAL',    'CHAR',   'ID',
    'SLASH',    'HEXSTRING'
]

# Set up token names.  This is always required.
tokens = []

def ignore_comments(ignore = True):
    global tokens, get_comments
    del tokens[:]   # clear the tokens list in situ
    tokens += keywords + other_tokens
    get_comments = not ignore
    if not ignore:
        tokens.append('COMMENT')

ignore_comments() # set up everything!

# Declare the states
states = (
    ('comment','exclusive'),
    ('hexstring','exclusive')
)

t_ignore = ' \t'

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

# Comment state section

# Match the first (*. Enter comment state.
def t_comment(t):
    r'\(\*'
    t.lexer.comment_start = t.lexer.lexpos     # Record the starting position
    t.lexer.level = 1                          # Initial comment level
    t.lexer.begin('comment')                   # Enter 'comment' state

# Rules for the comment state

# Note: there is no ignore rule here. lex will warn about it, but it's OK.

def t_comment_start(t):
    r'\(\*'
    t.lexer.level += 1

def t_comment_end(t):
    r'\*\)'
    global get_comments
    t.lexer.level -= 1
    # If closing star-paren, return the comment content
    if t.lexer.level == 0:
        t.value = t.lexer.lexdata[t.lexer.comment_start:t.lexer.lexpos-2]
        t.type = "COMMENT"
        t.lexer.lineno += t.value.count('\n')
        t.lexer.begin('INITIAL')
        if get_comments:
            return t

# For bad characters, we just skip over it
def t_comment_error(t):
    t.lexer.skip(1)

# Hex string section

def t_hexstring(t):
    r'\$'
    t.lexer.hexstring = ''
    t.lexer.begin('hexstring')                 # Enter 'hexstring' state

t_hexstring_ignore = ' \t'

def t_hexstring_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

# Rules for the comment state

def t_hexstring_hexdigits(t):
    r'[0-9A-F]{2}'
    t.lexer.hexstring += chr(int(t.value,16))

def t_hexstring_end(t):
    r'\$'
    t.value = t.lexer.hexstring
    t.type = "HEXSTRING"
    t.lexer.begin('INITIAL')
    return t

# For bad characters, we just skip over it
def t_hexstring_error(t):
    print("Illegal character '%s'" % (t.value[0],))
    t.lexer.skip(1)

# Normal state section

# Operators
t_NE      = r'\#'
t_AND     = r'&'
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_STAR    = r'\*'
t_PLUS    = r'\+'
t_COMMA   = r','
t_MINUS   = r'-'
t_UPTO    = r'\.\.'
t_DOT     = r'\.'
t_SLASH   = r'/'
t_BECOMES = r':='
t_COLON   = r':'
t_SEMI    = r';'
t_LE      = r'<='
t_LT      = r'<'
t_EQ      = r'='
t_GE      = r'>='
t_GT      = r'>'
t_LBRACE  = r'\{'
t_BAR     = r'\|'
t_RBRACE  = r'\}'
t_TILDE   = r'\~'
t_LBRAK   = r'\['
t_RBRAK   = r'\]'
t_ARROW   = r'\^'

def t_STRING(t):
    r'\".*?\"'
    t.value = t.value[1:-1]  # removes the quotes :-)
    return t

def t_ID(t):
    r'[A-Za-z][0-9A-Za-z]*'
    if t.value in keywords:
        t.type = t.value
    return t

def t_REAL(t):
    r'[0-9]+\.[0-9]+([DE][+-]?[0-9]+)?'
    try:
        t.value = float(t.value.replace('D', 'E'))
    except:
        print("Bad number")
        t.value = 0.0
    return t

def t_CHAR(t):
    r'[0-9][0-9A-F]*X'
    v = int(t.value[:-1], 16)
    if v > 255:
        print("Bad character")
        v = 0
    t.value = chr(v)
    return t

def t_INTEGER(t):
    r'[0-9][0-9A-F]*[HR]?'
    if t.value[-1] == 'H':
        t.value = int(t.value[:-1], 16)
    elif t.value[-1] == 'R':
        # I assume the original platform is big-endian, so...
        t.type = 'REAL'
        v = int(t.value[:-1],16)
        t.value = struct.unpack('>f',struct.pack('>i',v))[0]
    else:
        try:
            t.value = int(t.value)
        except:
            print("bad integer: %s" % (t.value,))
    return t

# The characters ! % ' ? @ _ ` 7FX are considered bad here

def t_error(t):
    print("Illegal character %s" % t.value[0])
    t.lexer.skip(1)

lexer = lex.lex(debug=False)

if __name__ == '__main__':
    lex.runmain()
