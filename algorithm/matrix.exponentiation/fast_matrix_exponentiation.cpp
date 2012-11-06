#include <iostream>
#include <cstdio>

using namespace std;

template<typename T>
class Matrix
{
public:
    Matrix(int m, int n);
    Matrix(Matrix<T> const& matrix);
    ~Matrix();

    Matrix<T> const& operator=(const Matrix<T> &A);
    Matrix<T> const  operator*(const Matrix<T> &A);
    Matrix<T> const  operator^(int P);

public:
    int m,n;
    T* data;
};

template<typename T>
Matrix<T>::Matrix(int m, int n)
{
    this->m = m;
    this->n = n;
    data = new T[m * n];
}

template<typename T>
Matrix<T>::Matrix(Matrix<T> const& A)
{
    this->m = A.m;
    this->n = A.n;
    data = new T[m*n];
    for (int i = 0; i <m * n; i++)
        data[i] = A.data[i];
}

template<typename T>
Matrix<T>::~Matrix()
{
    delete [] data;
}

template<typename T>
Matrix<T> const& Matrix<T>::operator=(Matrix<T> const& A)
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

template<typename T>
Matrix<T> const Matrix<T>::operator*(Matrix<T> const& A)
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

template<typename T>
const Matrix<T> Matrix<T>::operator^(int P)
{
    if (P == 1) 
        return (*this);

    if (P & 1) 
        return (*this) * ((*this) ^ (P-1));

    Matrix B = (*this) ^ (P/2);

    return B*B;
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
