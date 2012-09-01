#!/usr/bin/env python

def fibonacci(n):
    """docstring for fibonacci"""
    if n == 0:
        return 0
        
    if n == 1:
        return 1
    
    return fibonacci(n-1) + fibonacci(n-2)
    

print fibonacci(3)
        