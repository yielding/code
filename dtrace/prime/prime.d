#!/usr/sbin/dtrace -s

#pragma D option quiet

primes*:::primecalc-start
{
    self->start = timestamp;
}

primes*:::primecalc-done
/arg1 == 1/
{
    @times["prime"] = sum(timestamp - self->start);
}

primes*:::primecalc-done
/arg1 == 0/
{
    @times["nonprime"] = sum(timestamp - self->start);
}

END
{
    normalize(@times,1000000);
    printa(@times);
}
