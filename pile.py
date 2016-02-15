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



# Pile data structure

# (should now be usable for records, scopes, blocks and modules)

# Each entry of 'layers' is a pair: a list containing
# some information about that layer (the first entry
# MUST be a reference to an item), and a list of items
# (which in the simplest case can be strings or number).

# Note: The concept of level or activation is NOT explicitly
# encoded. In my PyOberon compiler, level info is obtained
# by getting second item (after the reference).
# To get a stack-like snapshot, use the
# get_stack_image() method, with appropriate layer
# indexing as its parameter if desired.

class Layer:
    def __init__(self, parent=[None,None]):
        self.parent = parent
        self.items = []

    def add(self, item):
        self.items.append(item)

    def find(self, etc):
        pass

class Pile:
    def __init__(self):
        # create first usable layer
        # make a list containing first pair representing the first layer
        self.layers = [ Layer() ]
        self.active_layers = [0] # make it refer to first working layer

    def get_current_layer(self):
        return self.active_layers[-1]

    def get_current_position(self):
        # returns the current position being worked on
        working_layer = self.get_current_layer()
        working_index = len(self.layers[working_layer].items)-1
        return [working_layer, working_index]

    def open_sublayer(self):
        # add a new layer, with some linkbacks
        new_layer = Layer(self.get_current_position())
        self.layers.append(new_layer)
        # mark new layer as the current active one
        self.active_layers.append(len(self.layers)-1)

    def close_sublayer(self):
        # return old "current" information
        return self.active_layers.pop()

    def add(self, item):
        working_layer = self.get_current_layer()
        self.layers[working_layer].add(item)

    def find(self, item, seek=lambda _list,obj:_list.index(obj), next=lambda layer,index: [layer,index]):
        search_layer = self.get_current_layer()
        while search_layer != None:
            try:
                return search_layer, seek(self.layers[search_layer].items, item)
            except ValueError:
                parent_layer, parent_index = self.layers[search_layer].parent
                if parent_layer == None:
                    search_layer = None
                else:
                    search_layer, search_index = next(parent_layer, parent_index)
        return [None, None]

    def get_stack_image(self, layer=None):
        # create and return a stack-like image
        stack = []
        if layer == None:
            layer = self.get_current_layer()
        index = len(self.layers[layer].items)-1
        while layer != None:
            stack.insert(0, self.layers[layer].items[:index+1])
            layer, index = self.layers[layer].parent
        return stack

### quick testing suite

if __name__ == '__main__':
    test = '''\
        system oberon object
        ( version fixorg code data
          Put0 ( op a b c temp )
          Put1 ( op a b imm )
          FixLink ( L | L1 )
          record ( parent | f1 f2 f3 )
          Big ( a1 a2 | m1 m2 m3
            Little ( b1 b2 | g1 g2 )
            stuff
          )
          morerecord ( | j1 j2 j3 j4 )
        ) main
    '''.split()

    p = Pile()
    for t in test:
        if t == '(':
            p.open_sublayer()
        elif t == ')':
            junk = p.close_sublayer()
        elif t == '|':
            junk = p.close_sublayer()
            p.open_sublayer()
        else:
            p.add(t)

    # verify that it indeed gives a sequence of stack-like
    # behavior in semi-chronological order:
    for pl in range(len(p.layers)):
        print 'Stack at:', pl, p.get_stack_image(layer=pl)

    for k in 'main oberon data system xyz'.split():
        print p.find(k)
