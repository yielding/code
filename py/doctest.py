#!/usr/bin/env python

def split(line, types=None, delimiter=None):
    """ split text and perform additional type conversion

    >>> split('GOOG 100 490,50')
    ['GOOG', '100', '490.50']
    >>> split('GOOG 100 490.50', [str, int, float])
    ['GOOG', '100', '490.50']
    >>>
    """
    fields = line.split(delimiter)
    if types:
        fields = [ty(val) for ty, val in zip(types, fields)]
    
    return fields

if __name__ == '__main__':
    import doctest
    doctest.testmod()
