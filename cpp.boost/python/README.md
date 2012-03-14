Building Python Embedding
=========================
g++ ./embedding.cpp -I/opt/local/Library/Frameworks/Python.framework/Versions/2.7/include/python2.7 
                    -L//opt/local/Library/Frameworks/Python.framework/Versions/2.7/lib -lpython2.7 -lboost_python
