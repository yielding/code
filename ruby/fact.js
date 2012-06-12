function fact(n)
{
    return (n > 1) ? n * fact(n-1) : 1;
}

b = 0;
for (var i=0; i<10000000; ++i) {
    b += fact(10);
}

console.log(b);
