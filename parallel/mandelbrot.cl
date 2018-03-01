
#pragma OPENCL_EXTENSION cl_khr_fp64 : enable

typedef double2 Complex;

Complex multiply(Complex a, Complex b);
float boundedorbit(Complex seed, Complex c, float bound, int bailout);
float normalized_iterations(int n, Complex zn, int bailout);
unsigned char grayvalue(float n);

Complex multiply(Complex a, Complex b) {
    return (Complex)(a.s0*b.s0-a.s1*b.s1, a.s1*b.s0+a.s0*b.s1);
}

float normalized_iterations(int n, Complex zn, int bailout) {
    return n + (log(log(convert_float(bailout)))-log(log(length(zn))))/log(2.0);
}

float boundedorbit(Complex seed, Complex c, float bound, int bailout)
{
    Complex z = multiply(seed, seed) + c;
    for (int k = 0; k < bailout; k++) {
        if (length(z) > bound) 
            return normalized_iterations(k, z, bailout);
        z = multiply(z, z) + c;
    }
    return FLT_MIN;
}

unsigned char grayvalue(float n) {
    return convert_uchar_sat_rte(n);
}

// TODO should make iteration count a parameter
__kernel void mandelbrot(__global unsigned char* output, size_t offset)
{
    int k = get_global_id(0);
    int j = get_global_id(1);
    
    // construct position
    Complex c = (Complex)(-2.5 + 3.5*(k/3500.0), -1.25 + 2.5*((j+offset)/2500.0));

    float count = boundedorbit((Complex)(0,0), c, 2.0, 200);

    // TODO remove guard
    //if (3500*j+k < 3500*2500)
    output[3500*j+k] = grayvalue(count);
}
