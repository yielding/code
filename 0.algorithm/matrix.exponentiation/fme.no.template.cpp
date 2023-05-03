#include <iostream>
#include <cstdio>

using namespace std;

class Matrix
{
public:
    Matrix(int m, int n);
    Matrix(Matrix const& matrix);
    ~Matrix();

    Matrix const& operator=(Matrix const& A);
    Matrix const  operator*(Matrix const& A);
    Matrix const  operator^(int P);

public:
    int m,n;
    T* data;
};

Matrix::Matrix(int m, int n)
{
    this->m = m;
    this->n = n;
    data = new T[m * n];
}

Matrix::Matrix(Matrix const& A)
{
    if (this != &A)
    {
        this->m = A.m;
        this->n = A.n;
        data = new T[m*n];
        for (int i = 0; i <m * n; i++)
            data[i] = A.data[i];
    }
}

Matrix::~Matrix()
{
    delete [] data;
}

Matrix const& Matrix::operator=(Matrix const& A)
{
    if (&A != this)
    {
        delete [] data;
        m = A.m;
        n = A.n;
        data = new T[m*n];
        for (int i=0; i < m*n; i++)
            data[i] = A.data[i];
    }

    return *this;
}

Matrix const Matrix::operator*(Matrix const& A)
{
    Matrix C(m, A.n);

    for (int i=0; i<m; ++i)
        for (int j=0; j<A.n; ++j)
        {
            C.data[i*C.n+j] = 0;
            for (int k = 0; k <n; ++k)
                C.data[i*C.n+j] = C.data[i*C.n+j] + data[i*n+k]*A.data[k*A.n+j];
        }

    return C;
}

Matrix const Matrix::operator^(int P)
{
    if (P == 1) 
        return (*this);

    if (P & 1) 
        return (*this) * ((*this) ^ (P-1));

    Matrix B = (*this) ^ (P/2);

    return B * B;
}

int main()
{
    Matrix<int> M(2,2);

    M.data[0] = 1; M.data[1] = 1;
    M.data[2] = 1; M.data[3] = 0;

    int F[2]={0,1};
    int N;

    while (~scanf("%d",&N))
        if (N>1)
            printf("%lld\n",(M^N).data[0]);
        else
            printf("%d\n",F[N]);

    return 0;
}

