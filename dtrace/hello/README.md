How to run
==========
sudo dtrace -q -c ./helloworld -n '
world$target:::loop {
       printf("%s:%s loop %d\n", probemod, probefunc, arg0);
}'
