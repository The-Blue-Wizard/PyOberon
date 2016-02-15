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


# PyOberon scanner/parser/analyzer tester (analyzer is ignored for now)
# In the future it will be expanded to test other parts like optimizer

import sys
#sys.path.insert(0,r"C:\Users\user1\wbench\ply-3.8")

if sys.version_info[0] >= 3:
    raw_input = input

#import parser2 as parser
import oberonparser
# I want to import analyzer just to force Python to find syntax mistakes there
import analyzer
import pprint

# Set up a logging object
import logging

logging.basicConfig(
    level = logging.DEBUG,
    filename = "parselog.txt",
    filemode = "w",
    format = "%(filename)10s:%(lineno)4d: %(message)s"
)
log = logging.getLogger()

print('''\
    PyOberon - Copyright (C) 2016  John "The Blue Wizard" Rogers
    This program comes with ABSOLUTELY NO WARRANTY; for details read LICENSE
    file. This is free software, and you are welcome to redistribute it
    under certain conditions.
''')

if len(sys.argv) > 1:
    f = open(sys.argv[1], 'rt')
    s = f.read()
    f.close()
    fout = open('testout.txt','wt')
    print('Now parsing', sys.argv[1])
    t = oberonparser.parse(s,debug=log)
    pprint.pprint(t,fout)
    fout.close()
    #u = analyzer.analyze(t)
else:
    while True:
        try:
            s = raw_input('Oberon > ')
        except EOFError:
            break
        if not s: continue
        t = oberonparser.parse(s,debug=log)
        pprint.pprint(t)
        #u = analyzer.analyze(t)
