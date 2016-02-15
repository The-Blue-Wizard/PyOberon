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


# Translated from several sources, including ORP.Mod (N. Wirth 1.7.97 / 7.6.2014
#  Oberon 07 parser for RISC in Python

# This version of Oberon parser does parsing of source codes, with a few differences.
# It uses scanner.py to get symbols, and it uses yacc from PLY to parse them. The
# output is an AST. Note that unlike ORP, it does not do any type checking, nor
# generate code. that is being done by modules analyzer.py, optimizer.py and
# codegen/py.

# see http://www.cs.may.ie/~jpower/Oberon/oberon.y (it's for Oberon-2 though)

import sys
sys.path.insert(0,r"C:\Users\user1\wbench\ply-3.8")
from ply import *
import scanner

tokens = scanner.tokens

precedence = (
               ('nonassoc', 'EQ', 'NE', 'LT', 'LE', 'GT', 'GE', 'IN', 'IS'),
               ('left',  'PLUS', 'MINUS', 'OR'),
               ('right', 'UPLUS', 'UMINUS'),
               ('left',  'STAR', 'SLASH', 'DIV', 'MOD', 'AND'),
               ('right', 'TILDE')
)

# support routines

def starred_as(s, text):
    if s == '*':
        return text
    return ''

#### module

def p_module(p):
    '''module : MODULE star_opt ident SEMI import_statement_opt declaration_sequence body_opt END ident DOT'''
    version = starred_as(p[2], 'OLD')
    p[0] = ['MODULE', version, p[3], p[5], p[6], p[7], p[9]]

#### import statement

def p_import_statement_opt_0(p):
    '''import_statement_opt : IMPORT import_list SEMI'''
    p[0] = p[2]

def p_import_statement_opt_1(p):
    '''import_statement_opt : '''
    # empty import
    p[0] = None

# import list

def p_import_list_0(p):
    '''import_list : import_list COMMA module_name'''
    p[0] = p[1] + [p[3]]

def p_import_list_1(p):
    '''import_list : module_name'''
    p[0] = [p[1]]

# import name

def p_module_name_0(p):
    '''module_name : ident BECOMES ident'''
    p[0] = [p[1], p[3]]

def p_module_name_1(p):
    '''module_name : ident'''
    p[0] = [p[1], p[1]]

#### declaration sequence

def p_declaration_sequence(p):
    '''declaration_sequence : constants types variables procedures'''
    p[0] = [p[1], p[2], p[3], p[4]]

#### constants

def p_constants_0(p):
    '''constants : CONST constant_list'''
    p[0] = p[2]

def p_constants_1(p):
    '''constants : '''
    p[0] = None

# constant list

def p_constant_list_0(p):
    '''constant_list : constant_list constant_declaration SEMI'''
    p[0] = p[1] + [p[2]]

def p_constant_list_1(p):
    '''constant_list : constant_declaration SEMI'''
    p[0] = [p[1]]

# constant declaration

def p_constant_declaration(p):
    '''constant_declaration : identdef EQ constant_expr'''
    p[0] = [p[1], p[3]]

#### types

def p_types_0(p):
    '''types : TYPE type_list'''
    p[0] = p[2]

def p_types_1(p):
    '''types : '''
    p[0] = None

# type list

def p_type_list_0(p):
    '''type_list : type_list type_declaration'''
    p[0] = p[1] + [p[2]]

def p_type_list_1(p):
    '''type_list : type_declaration'''
    p[0] = [p[1]]

# type declaration

# Note: A careful examination reveal the discrepancy in the
# type declaration syntax as defined in Oberon 07 grammar and
# that found in ORP.Mod's Type0 routine, namely that it parses
# qualident in addition to StrucType. It pays to have the
# original source code at hand!

def p_type_declaration(p):
    '''type_declaration : identdef EQ type SEMI'''
    # was: '''type_declaration : identdef EQ structure_type SEMI'''
    p[0] = [p[1], p[3]]

# structure type

def p_structure_type_0(p):
    '''structure_type : array_type'''
    p[0] = p[1]

def p_structure_type_1(p):
    '''structure_type : record_type'''
    p[0] = p[1]

def p_structure_type_2(p):
    '''structure_type : pointer_type'''
    p[0] = p[1]

def p_structure_type_3(p):
    '''structure_type : procedure_type'''
    p[0] = p[1]

# array type

# Note: The analyzer will expand "ARRAY n1, n2,
# ..., nk OF not_array_type" into "ARRAY n1 OF
# ARRAY n2 OF ... ARRAY nk OF not_array_type"
# and create appropriate AST.

def p_array_type_0(p):
    '''array_type : ARRAY constant_expr_list OF type'''
    p[0] = ['ARRAY', p[2], p[4]]

def p_array_type_1(p):
    '''array_type : ARRAY OF type'''
    p[0] = ['ARRAY', None, p[3]]

def p_constant_expr_list_0(p):
    '''constant_expr_list : constant_expr_list COMMA constant_expr'''
    p[0] = p[1] + [p[3]]

def p_constant_expr_list_1(p):
    '''constant_expr_list : constant_expr'''
    p[0] = [p[1]]

# record type

# Note that this deviates a bit from the Oberon
# grammar documentation. This is confirmed by
# looking at the source code in ORP.Mod.Txt (in
# procedure RecordType), and the record TypeDesc
# in the ORB.Mod.Txt file has this extra semicolon.
# Therefore this parser must support that.

def p_record_type_0(p):
    '''record_type : RECORD base_type_opt field_list semi_opt END'''
    p[0] = ['RECORD', p[2], p[3]]

def p_record_type_1(p):
    '''record_type : RECORD base_type_opt END'''
    p[0] = ['RECORD', p[2], None]

def p_base_type_opt_0(p):
    '''base_type_opt : LPAREN qualident RPAREN'''
    p[0] = p[2]

def p_base_type_opt_1(p):
    '''base_type_opt : '''
    p[0] = None

def p_field_list_0(p):
    '''field_list : field_list SEMI field'''
    p[0] = p[1] + [p[3]]

def p_field_list_1(p):
    '''field_list : field'''
    p[0] = [p[1]]

def p_field(p):
    '''field : ident_list COLON type'''
    p[0] = [p[1], p[3]]

def p_semi_opt_0(p):
    '''semi_opt : SEMI'''
    p[0] = ';'

def p_semi_opt_1(p):
    '''semi_opt : '''
    p[0] = ''

# pointer type

def p_pointer_type(p):
    '''pointer_type : POINTER TO type'''
    p[0] = ['POINTER', p[3]]

# procedure type

def p_procedure_type(p):
    '''procedure_type : PROCEDURE formal_parameters_opt'''
    p[0] = ['PROCEDURE', p[2]]

def p_formal_parameters_opt_0(p):
    '''formal_parameters_opt : formal_parameters'''
    p[0] = p[1]

def p_formal_parameters_opt_1(p):
    '''formal_parameters_opt : '''
    p[0] = None

def p_formal_parameters_0(p):
    '''formal_parameters : LPAREN FP_list_opt RPAREN COLON qualident'''
    p[0] = [p[2], p[4]]

def p_formal_parameters_1(p):
    '''formal_parameters : LPAREN FP_list_opt RPAREN'''
    p[0] = [p[2], None]

def p_FP_list_opt_0(p):
    '''FP_list_opt : FP_list'''
    p[0] = p[1]

def p_FP_list_opt_1(p):
    '''FP_list_opt : '''
    p[0] = None

def p_FP_list_0(p):
    '''FP_list : FP_list SEMI formal_param'''
    p[0] = p[1] + [p[3]]

def p_FP_list_1(p):
    '''FP_list : formal_param'''
    p[0] = [p[1]]

def p_formal_param(p):
    '''formal_param : VAR_opt fp_ident_list COLON formal_type'''
    p[0] = [p[2], p[1], p[3]]

def p_VAR_opt_0(p):
    '''VAR_opt : VAR'''
    p[0] = 'VAR'

def p_VAR_opt_1(p):
    '''VAR_opt : '''
    p[0] = None

def p_fp_ident_list_0(p):
    '''fp_ident_list : fp_ident_list COMMA fp_ident'''
    p[0] = p[1] + [p[3]]

def p_fp_ident_list_1(p):
    '''fp_ident_list : fp_ident'''
    p[0] = [p[1]]

def p_fp_ident(p):
    '''fp_ident : ident'''
    p[0] = p[1]

def p_formal_type(p):
    '''formal_type : array_of_list qualident'''
    p[0] = [p[2], p[1]]

def p_array_of_list_0(p):
    '''array_of_list : array_of_list ARRAY OF'''
    p[0] = p[1]+1

def p_array_of_list_1(p):
    '''array_of_list : '''
    p[0] = 0

#### variables

def p_variables_0(p):
    '''variables : VAR variable_list'''
    p[0] = p[2]

def p_variables_1(p):
    '''variables : '''
    p[0] = None

# variable list

def p_variable_list_0(p):
    '''variable_list : variable_list variable_declaration SEMI'''
    p[0] = p[1] + [p[2]]

def p_variable_list_1(p):
    '''variable_list : variable_declaration SEMI'''
    p[0] = [p[1]]

# variable declaration

def p_variable_declaration(p):
    '''variable_declaration : ident_list COLON type'''
    p[0] = [p[1], p[3]]

def p_ident_list_0(p):
    '''ident_list : ident_list COMMA identdef'''
    p[0] = p[1] + [p[3]]

def p_ident_list_1(p):
    '''ident_list : identdef'''
    p[0] = [p[1]]

# type

def p_type_0(p):
    '''type : qualident'''
    p[0] = p[1]

def p_type_1(p):
    '''type : structure_type'''
    p[0] = p[1]

#### procedures

def p_procedures_0(p):
    '''procedures : procedures procedure_declaration SEMI'''
    p[0] = p[1] + [p[2]]

def p_procedures_1(p):
    '''procedures : '''
    p[0] = []

def p_procedure_declaration(p):
    '''procedure_declaration : procedure_heading SEMI procedure_body ident'''
    p[0] = [p[1], p[3], p[4]]

def p_procedure_heading(p):
    '''procedure_heading : PROCEDURE star_opt identdef formal_parameters_opt'''
    p[0] = ['PROCEDURE', starred_as(p[2], 'INTERRUPT'), p[3], p[4]]

def p_procedure_body(p):
    '''procedure_body : declaration_sequence body_opt procedure_return_opt END'''
    p[0] = [p[1], p[2], p[3]]

def p_procedure_return_opt_0(p):
    '''procedure_return_opt : RETURN expr'''
    p[0] = p[2]

def p_procedure_return_opt_1(p):
    '''procedure_return_opt : '''
    p[0] = None

#### body

def p_body_opt_0(p):
    '''body_opt : BEGIN statement_sequence'''
    p[0] = p[2]

def p_body_opt_1(p):
    '''body_opt : '''
    # empty body
    p[0] = None

# statement sequence

def p_statement_sequence_0(p):
    '''statement_sequence : statement_sequence SEMI statement'''
    p[0] = p[1] + [p[3]]

def p_statement_sequence_1(p):
    '''statement_sequence : statement'''
    p[0] = [p[1]]

# statement

def p_statement_0(p):
    '''statement : assignment'''
    p[0] = p[1]

def p_statement_1(p):
    '''statement : procedure_call'''
    p[0] = p[1]

def p_statement_2(p):
    '''statement : if_statement'''
    p[0] = p[1]

def p_statement_3(p):
    '''statement : case_statement'''
    p[0] = p[1]

def p_statement_4(p):
    '''statement : while_statement'''
    p[0] = p[1]

def p_statement_5(p):
    '''statement : repeat_statement'''
    p[0] = p[1]

def p_statement_6(p):
    '''statement : for_statement'''
    p[0] = p[1]

def p_statement_7(p):
    '''statement : '''
    # empty statement
    p[0] = []

# assignment

def p_assignment(p):
    '''assignment : designator BECOMES expr'''
    p[0] = ['ASSIGN', p[1], p[3]]

# procedure call

def p_procedure_call_0(p):
    '''procedure_call : designator actual_parameters'''
    p[0] = ['CALL', p[1], p[2]]

def p_procedure_call_1(p):
    '''procedure_call : designator'''
    p[0] = ['CALL', p[1], None]

def p_actual_parameters_0(p):
    '''actual_parameters : LPAREN expr_list RPAREN'''
    p[0] = p[2]

def p_actual_parameters_1(p):
    '''actual_parameters : LPAREN RPAREN'''
    p[0] = [None]

# if statement

def p_if_statement(p):
    '''if_statement : IF if_condition_clause_list else_opt END'''
    p[0] = ['IF'] + p[2]
    if p[3] != []:
        p[0].append(p[3])

def p_if_condition_clause_list_0(p):
    '''if_condition_clause_list : if_condition_clause_list ELSIF if_condition_clause'''
    p[0] = p[1] + [p[3]]

def p_if_condition_clause_list_1(p):
    '''if_condition_clause_list : if_condition_clause'''
    p[0] = [p[1]]

def p_if_condition_clause(p):
    '''if_condition_clause : expr THEN statement_sequence'''
    p[0] = [p[1], p[3]]

def p_else_opt_0(p):
    '''else_opt : ELSE statement_sequence'''
    p[0] = [None, p[2]]

def p_else_opt_1(p):
    '''else_opt : '''
    p[0] = []

# case statement

def p_case_statement(p):
    '''case_statement : CASE expr OF case_list END'''
    p[0] = ['CASE', p[2]] + p[4]

def p_case_list_0(p):
    '''case_list : case_list BAR case'''
    p[0] = p[1] + [p[3]]

def p_case_list_1(p):
    '''case_list : case'''
    p[0] = [p[1]]

def p_case(p):
    '''case : case_label_list COLON statement_sequence'''
    p[0] = [p[1], p[3]]

def p_case_label_list_0(p):
    '''case_label_list : case_label_list COMMA label_range'''
    p[0] = p[1] + [p[3]]

def p_case_label_list_1(p):
    '''case_label_list : label_range'''
    p[0] = [p[1]]

def p_label_range_0(p):
    '''label_range : label UPTO label'''
    p[0] = [p[1], p[3]]

def p_label_range_1(p):
    '''label_range : label'''
    p[0] = [p[1], None]

def p_label_0(p):
    '''label : INTEGER'''
    p[0] = p[1]

def p_label_1(p):
    '''label : STRING'''
    p[0] = p[1]

def p_label_2(p):
    '''label : ID'''
    p[0] = p[1]

# while statement

def p_while_statement(p):
    '''while_statement : WHILE while_condition_clause_list END'''
    p[0] = ['WHILE'] + p[2]

def p_while_condition_clause_list_0(p):
    '''while_condition_clause_list : while_condition_clause_list ELSIF while_condition_clause'''
    p[0] = p[1] + [p[3]]

def p_while_condition_clause_list_1(p):
    '''while_condition_clause_list : while_condition_clause'''
    p[0] = [p[1]]

def p_while_condition_clause(p):
    '''while_condition_clause : expr DO statement_sequence'''
    p[0] = [p[1], p[3]]

# repeat statement

def p_repeat_statement(p):
    '''repeat_statement : REPEAT statement_sequence UNTIL expr'''
    p[0] = ['REPEAT', p[2], p[4]]

# for statement

def p_for_statement(p):
    '''for_statement : FOR ident BECOMES expr TO expr by_opt DO statement_sequence END'''
    p[0] = ['FOR', p[2], p[4], p[6], p[7], p[8]]

def p_by_opt_0(p):
    '''by_opt : BY constant_expr'''
    p[0] = p[2]

def p_by_opt_1(p):
    '''by_opt : '''
    p[0] = None

# constant expr

def p_constant_expr(p):
    '''constant_expr : expr'''
    p[0] = p[1]

# expr

def p_expr_0(p):
    '''expr : expr LT expr'''
    p[0] = ['LT', p[1], p[3]]

def p_expr_1(p):
    '''expr : expr LE expr'''
    p[0] = ['LE', p[1], p[3]]

def p_expr_2(p):
    '''expr : expr GT expr'''
    p[0] = ['GT', p[1], p[3]]

def p_expr_3(p):
    '''expr : expr GE expr'''
    p[0] = ['GE', p[1], p[3]]

def p_expr_4(p):
    '''expr : expr EQ expr'''
    p[0] = ['EQUALS', p[1], p[3]]

def p_expr_5(p):
    '''expr : expr NE expr'''
    p[0] = ['NE', p[1], p[3]]

def p_expr_6(p):
    '''expr : expr AND expr'''
    p[0] = ['AND', p[1], p[3]]

def p_expr_7(p):
    '''expr : expr OR expr'''
    p[0] = ['OR', p[1], p[3]]

def p_expr_8(p):
    '''expr : expr IN expr'''
    p[0] = ['IN', p[1], p[3]]

def p_expr_9(p):
    '''expr : expr IS expr'''
    p[0] = ['IS', p[1], p[3]]

def p_expr_10(p):
    '''expr : expr MOD expr'''
    p[0] = ['MOD', p[1], p[3]]

def p_expr_11(p):
    '''expr : expr PLUS expr'''
    p[0] = ['ADD', p[1], p[3]]

def p_expr_12(p):
    '''expr : expr MINUS expr'''
    p[0] = ['SUBTRACT', p[1], p[3]]

def p_expr_13(p):
    '''expr : expr STAR expr'''
    p[0] = ['TIMES', p[1], p[3]]

def p_expr_14(p):
    '''expr : expr DIV expr'''
    p[0] = ['INT_DIVIDE', p[1], p[3]]

def p_expr_15(p):
    '''expr : expr SLASH expr'''
    p[0] = ['DIVIDE', p[1], p[3]]

def p_expr_16(p):
    '''expr : PLUS expr %prec UPLUS'''
    p[0] = ['UPLUS', p[2]]

def p_expr_17(p):
    '''expr : MINUS expr %prec UMINUS'''
    p[0] = ['UMINUS', p[2]]

def p_expr_18(p):
    '''expr : INTEGER'''
    p[0] = ['INTEGER', p[1]]

def p_expr_19(p):
    '''expr : REAL'''
    p[0] = ['REAL', p[1]]

def p_expr_20(p):
    '''expr : CHAR'''
    p[0] = ['CHAR', p[1]]

def p_expr_21(p):
    '''expr : STRING'''
    p[0] = ['STRING', p[1]]

def p_expr_22(p):
    '''expr : HEXSTRING'''
    p[0] = ['HEXSTRING', p[1]]

def p_expr_23(p):
    '''expr : NIL'''
    p[0] = ['NIL']

def p_expr_24(p):
    '''expr : TRUE'''
    p[0] = ['TRUE']

def p_expr_25(p):
    '''expr : FALSE'''
    p[0] = ['FALSE']

def p_expr_26(p):
    '''expr : set'''
    p[0] = p[1]

def p_expr_27(p):
    '''expr : designator actual_parameters'''
    # the VARFUNCCALL tag is used instead of
    # FUNCCALL to simplify the analysis a bit
    p[0] = ['VARFUNCCALL', p[1], p[2]]

def p_expr_28(p):
    '''expr : designator'''
    p[0] = ['VARFUNCCALL', p[1], None]

def p_expr_29(p):
    '''expr : LPAREN expr RPAREN'''
    p[0] = p[2]

def p_expr_30(p):
    '''expr : TILDE expr'''
    p[0] = ['NOT', p[2]]

def p_set_0(p):
    '''set : LBRACE element_list RBRACE'''
    p[0] = ['SET', p[2]]

def p_set_1(p):
    '''set : LBRACE RBRACE'''
    p[0] = ['SET', None]

def p_element_list_0(p):
    '''element_list : element_list COMMA element'''
    p[0] = p[1] + [p[3]]

def p_element_list_1(p):
    '''element_list : element'''
    p[0] = [p[1]]

def p_element_0(p):
    '''element : expr UPTO expr'''
    p[0] = [p[1], p[3]]

def p_element_1(p):
    '''element : expr'''
    p[0] = [p[1], None]

# designator

# parser can't tell whether the first ident is a module
# ident, so I modify the designator so it just use
# ident instead of qualident, and let the analyzer
# fix it

def p_designator(p):
    '''designator : ident selector_list'''
    # was: '''designator : qualident selector_list'''
    p[0] = ['DESIGNATOR', p[1]] + p[2]

# selector_list

def p_selector_list_0(p):
    '''selector_list : selector_list selector'''
    p[0] = p[1] + [p[2]]

def p_selector_list_1(p):
    '''selector_list : '''
    p[0] = []

# selector

def p_selector_0(p):
    '''selector : DOT ident'''
    p[0] = ['DOT', p[2]]

def p_selector_1(p):
    '''selector : LBRAK expr_list RBRAK'''
    p[0] = ['ARRAYINDICES', p[2]]

def p_selector_2(p):
    '''selector : ARROW'''
    p[0] = ['ARROW']

# parser can't tell between procedure/function call
# and this, so I comment it out, and let the analyzer
# fix it

# def p_selector_3(p):
    # '''selector : LPAREN qualident RPAREN'''
    # p[0] = ['TYPEGUARD', p[2]]

# expr list

def p_expr_list_0(p):
    '''expr_list : expr_list COMMA expr'''
    p[0] = p[1] + [p[3]]

def p_expr_list_1(p):
    '''expr_list : expr'''
    p[0] = [p[1]]

# qualident

def p_qualident_0(p):
    '''qualident : ident DOT ident'''
    p[0] = [p[1], p[3]]

def p_qualident_1(p):
    '''qualident : ident'''
    p[0] = [None, p[1]]

# identdef

def p_identdef(p):
    '''identdef : ident star_opt'''
    p[0] = [p[1], starred_as(p[2], 'EXPORTED')]

def p_star_opt_0(p):
    '''star_opt : STAR'''
    p[0] = p[1]

def p_star_opt_1(p):
    '''star_opt : '''
    p[0] = ''

# ident

def p_ident(p):
    '''ident : ID'''
    p[0] = p[1]

oberonparser = yacc.yacc()

def parse(data,debug=0):
    oberonparser.error = 0
    p = oberonparser.parse(data,debug=debug)
    if oberonparser.error:
        return None
    return p
