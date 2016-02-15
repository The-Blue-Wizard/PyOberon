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


# PyOberon analyzer

# It checks the parse tree for correct usage, and in a few
# cases, fixes the parse information to reflect actual
# semantic usage. It also reads symbol files using symbols module,
# and produces a symbol file using that module as well. Moreover,
# it builds a symbol table.

import sys

if sys.version_info[0] >= 3:
    raw_input = input

import symbols  # needed for obtaining external module info

def warn(m):
    print(m)

def error(m):
    print(m)

#-----------

# support routines

def is_extension(type1,type2):
    return type1 == type2 or (type2 != None and is_extension(type1,type2.base))

def equal_signatures(type1,type2):
    if type1.base != type2.base or type1.numofparams != type2.numofparams:
        return False
    # need more code here
    return True

def is_compatible(type1,type2):
    return type1 == type2

#-----------

def analyze_LT(p):
    s = analyze_expr(p[1])
    if s: pass
    t = analyze_expr(p[2])
    # do more testing
    return p

def analyze_LE(p):
    s = analyze_expr(p[1])
    t = analyze_expr(p[2])
    # do more testing
    return p

def analyze_GT(p):
    s = analyze_expr(p[1])
    t = analyze_expr(p[2])
    # do more testing
    return p

def analyze_GE(p):
    s = analyze_expr(p[1])
    t = analyze_expr(p[2])
    # do more testing
    return p

def analyze_EQUALS(p):
    s = analyze_expr(p[1])
    t = analyze_expr(p[2])
    # do more testing
    return p

def analyze_NE(p):
    s = analyze_expr(p[1])
    t = analyze_expr(p[2])
    # do more testing
    return p

def analyze_AND(p):
    s = analyze_expr(p[1])
    t = analyze_expr(p[2])
    verify_sametype(s,t)
    verify(s, 'BOOLEAN')
    return p

def analyze_OR(p):
    s = analyze_expr(p[1])
    t = analyze_expr(p[2])
    verify_sametype(s,t)
    verify(s, 'BOOLEAN')
    return p

def analyze_IN(p):
    s = analyze_expr(p[1])
    t = analyze_expr(p[2])
    # do more testing
    return p

def analyze_IS(p):
    s = analyze_expr(p[1])
    t = analyze_expr(p[2])
    # do more testing
    return p

def analyze_MOD(p):
    s = analyze_expr(p[1])
    t = analyze_expr(p[2])
    verify_sametype(s,t)
    verify(s, 'INTEGER')
    return p

def analyze_ADD(p):
    s = analyze_expr(p[1])
    t = analyze_expr(p[2])
    verify_sametype(s,t)
    verify(s, ['INTEGER', 'REAL'])
    return p

def analyze_SUBTRACT(p):
    s = analyze_expr(p[1])
    t = analyze_expr(p[2])
    verify_sametype(s,t)
    verify(s, ['INTEGER', 'REAL'])
    return p

def analyze_TIMES(p):
    s = analyze_expr(p[1])
    t = analyze_expr(p[2])
    verify_sametype(s,t)
    verify(s, ['INTEGER', 'REAL', 'SET'])
    return p

def analyze_INT_DIVIDE(p):
    s = analyze_expr(p[1])
    t = analyze_expr(p[2])
    verify_sametype(s,t)
    verify(s, 'INTEGER')
    return p

def analyze_DIVIDE(p):
    s = analyze_expr(p[1])
    t = analyze_expr(p[2])
    verify_sametype(s,t)
    verify(s, ['REAL', 'SET'])
    return p

def analyze_UPLUS(p):
    s = analyze_expr(p[1])
    verify(s, ['INTEGER', 'REAL', 'SET'])
    return p

def analyze_UMINUS(p):
    s = analyze_expr(p[1])
    verify(s, ['INTEGER', 'REAL', 'SET'])
    return p

def analyze_NIL(p):
    # no analysis needed
    return p

def analyze_TRUE(p):
    # no analysis needed
    return p

def analyze_FALSE(p):
    # no analysis needed
    return p

def analyze_VARFUNCCALL(p):
    s = analyze_DESIGNATOR(p[1])
    if p[2] != None:
        t = analyze_parameters(p[2])
    else:
        t = None
    # do more testing
    return p

def analyze_NOT(p):
    s = analyze_expr(p[1])
    verify(s, 'BOOLEAN')
    return p

def analyze_INTEGER(p):
    # no analysis needed
    return p

def analyze_REAL(p):
    # no analysis needed
    return p

def analyze_CHAR(p):
    # no analysis needed
    return p

def analyze_STRING(p):
    # no analysis needed
    return p

def analyze_HEXSTRING(p):
    # HEXSTRING is analyzed as STRING...that's it!
    p[0] = 'STRING'
    return p

def analyze_element(element):
    return element

def analyze_SET(p):
    if p[1] == None:
        return ['EMPTYSET']  # unsure about that for now
    # need more work
    for element in p[1]:
        element = analyze_element(element)
    return p

def analyze_DESIGNATOR(p):
    # first, check to see if the ident is a module name.
    # if so, make sure there is a DOT ID and convert it
    # to a qualident, then to an indexed ID
    item = lookup_ident(p[1])
    if item.klass == 'MODULE' and p[2] == 'DOT':
        module_item = lookup_ident(p[3], p[1])
        if module_ident == None:
            error('Qualified identifier expected after the module "%s"' % (p[1],))
            return p
    # then ....
    # do more testing
    return p

def analyze_expr(p):
    if p[0] == 'LT':
        p = analyze_LT(p)
    elif p[0] == 'LE':
        p = analyze_LE(p)
    elif p[0] == 'GT':
        p = analyze_GT(p)
    elif p[0] == 'GE':
        p = analyze_GE(p)
    elif p[0] == 'EQUALS':
        p = analyze_EQUALS(p)
    elif p[0] == 'NE':
        p = analyze_NE(p)
    elif p[0] == 'AND':
        p = analyze_AND(p)
    elif p[0] == 'OR':
        p = analyze_OR(p)
    elif p[0] == 'IN':
        p = analyze_IN(p)
    elif p[0] == 'IS':
        p = analyze_IS(p)
    elif p[0] == 'MOD':
        p = analyze_MOD(p)
    elif p[0] == 'ADD':
        p = analyze_ADD(p)
    elif p[0] == 'SUBTRACT':
        p = analyze_SUBTRACT(p)
    elif p[0] == 'TIMES':
        p = analyze_TIMES(p)
    elif p[0] == 'INT_DIVIDE':
        p = analyze_INT_DIVIDE(p)
    elif p[0] == 'DIVIDE':
        p = analyze_DIVIDE(p)
    elif p[0] == 'UPLUS':
        p = analyze_UPLUS(p)
    elif p[0] == 'UMINUS':
        p = analyze_UMINUS(p)
    elif p[0] == 'NIL':
        p = analyze_NIL(p)
    elif p[0] == 'TRUE':
        p = analyze_TRUE(p)
    elif p[0] == 'FALSE':
        p = analyze_FALSE(p)
    elif p[0] == 'VARFUNCCALL':
        p = analyze_VARFUNCCALL(p)
    elif p[0] == 'NOT':
        p = analyze_NOT(p)
    elif p[0] == 'INTEGER':
        p = analyze_INTEGER(p)
    elif p[0] == 'REAL':
        p = analyze_REAL(p)
    elif p[0] == 'CHAR':
        p = analyze_CHAR(p)
    elif p[0] == 'STRING':
        p = analyze_STRING(p)
    elif p[0] == 'HEXSTRING':
        p = analyze_HEXSTRING(p)
    elif p[0] == 'SET':
        p = analyze_SET(p)
    elif p[0] == 'DESIGNATOR':
        p = analyze_DESIGNATOR(p)
    # do more testing
    return p

def analyze_body(p):
    return p

def analyze_ASSIGN(p):
    s = analyze_DESIGNATOR(p[1])
    t = analyze_expr(p[2])
    return p

def analyze_CALL(p):
    s = analyze_DESIGNATOR(p[1])
    t = analyze_parameters(p[2])
    return p

def analyze_IF(p):
    for expr, body in p[1:]:
        if expr != None:
            s = analyze_expr(expr)
        else:
            s = None
        t = analyze_body(body)
    return p

def analyze_CASE(p):
    s = analyze_expr(p[1])
    for case in p[2:]:
        t = analyze_case_labels(case[0])
        u = analyze_body(case[1])
    return p

def analyze_WHILE(p):
    for expr, body in p[1:]:
        s = analyze_expr(expr)
        t = analyze_body(body)
    return p

def analyze_REPEAT(p):
    s = analyze_body(p[1])
    t = analyze_expr(p[2])
    return p

def analyze_FOR(p):
    # analyze p[1] as ident
    s = analyze_expr(p[2])
    t = analyze_expr(p[3])
    if p[4] != None:
        u = analyze_expr(p[4])
    else:
        u = None
    v = analyze_body(p[5])
    return p

def analyze_statement(p):
    if p[0] == 'ASSIGN':
        p = analyze_ASSIGN(p)
    elif p[0] == 'CALL':
        p = analyze_CALL(p)
    elif p[0] == 'IF':
        p = analyze_IF(p)
    elif p[0] == 'CASE':
        p = analyze_CASE(p)
    elif p[0] == 'WHILE':
        p = analyze_WHILE(p)
    elif p[0] == 'REPEAT':
        p = analyze_REPEAT(p)
    elif p[0] == 'FOR':
        p = analyze_FOR(p)
    return p

####

def analyze_constants(p):
    return p

def analyze_types(p):
    return p

def analyze_variables(p):
    return p

def analyze_procedures(p):
    return p

# The symbol table contains every identifier that is
# determined to be distinct (scope-level, order of
# appearance, etc.). I chose list instead of dictionary
# due to the necessity of declaring them in order.
# Of course the lookup is slow, but indexing would be
# fast.
symbol_table = []

# modules is a dictionary of imported module name and
# their indices (actually, TBD) into symbol table.
modules = {}

def analyze_import(p):
    global modules
    # for obvious reasons it has to read in the file
    # and add it to the list of information
    for import_alias, import_name in p:
        if modules.has_key(import_alias):
            pass
        else:
            if import_name == 'SYSTEM':
                # special case
                pass
            s = symbols.read_import()
            modules[import_alias] = 'junk' # FIXME
    return p

def analyze_declarations(p):
    p[0] = analyze_constants(p[0])
    p[1] = analyze_types(p[1])
    p[2] = analyze_variables(p[2])
    p[3] = analyze_procedures(p[3])
    return p

def analyze_body(p):
    if p != None:
        for k in range(len(p)):
            p[k] = analyze_statement(p[k])
    return p

def analyze_module(p):
    # checks that the module ident names match
    if p[0] != 'MODULE':
        error('MODULE tag expected')
    if p[2] != p[6]:
        warn('MODULE ident "%s" doesn\'t match its END ident "%s", "%s" is assumed' % (p[2], p[6], p[2]))
        p[6] = p[2]
    # ORP doesn't check module name against imported module names...
    # this routine might enforce that in the future (EXTRA)
    p[3] = analyze_import(p[3])
    p[4] = analyze_declarations(p[4])
    p[5] = analyze_body(p[5])
    return p

def analyze(p):
    if p == []:
        return None
    p = analyze_module(p)
    return p
