#!/usr/bin/env python2
# -*- coding: utf-8 -*-

def quicksort(arr) -> [int]:
    if len(arr) <= 1:
        return arr

    pivot = arr[len(arr) // 2]
    left   = [x for x in arr if x < pivot]
    middle = [x for x in arr if x == pivot]
    right  = [x for x in arr if x > pivot]

    return quicksort(left) + middle + quicksort(right)

print(quicksort([1, 3, 2, 4, 6]))
