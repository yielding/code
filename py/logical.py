#!/usr/bin/env python

import itertools 

def extract_vars(expression): 
    return sorted(set(c for c in expression if c.isupper())) 

def truth_table(expression): 
    vnames = extract_vars(expression) 
    print ' '.join(vnames), expression 
    for vs in itertools.product([True,False], repeat=len(vnames)): 
        context = dict(zip(vnames, vs)) 
        print ' '.join('T' if tf else 'F' for tf in vs + (eval(expression, context),)) 

truth_table('A and not (B or C)') 
#truth_table('((A and B) or (B and C))') 
#truth_table('(not(A and B) and (A and C))') 
#truth_table('((not A and B) or not C) and D') 
#truth_table('(not((not A and B) and (C or not D)))') 
