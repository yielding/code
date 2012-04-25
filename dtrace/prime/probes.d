provider primes {

    /* Start of the prime calculation */

    probe primecalc__start(long prime);

    /* End of the prime calculation */

    probe primecalc__done(long prime, int isprime);

    /* Exposes the size of the table of existing primes */

    probe primecalc__tablesize(long tablesize);

};
