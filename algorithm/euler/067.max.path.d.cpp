#include <iostream>
#include <vector>
#include <ctime>
#include <sstream>
#include <stdint.h>
#include <iomanip>

using namespace std;

typedef vector<vector<int64_t>> Grid;

class Tracer
{
public:
    Tracer(Grid & m)
    {
        _max_seq = m[m.size() -1];
        _trace.resize(m.size());

        for (int i=0; i<_max_seq.size(); i++)
            _trace[i].push_back(_max_seq[i]);
    }

    int64_t max_value()
    {
        return _max_seq[0];
    }

    string max_trace()
    {
        // 순서를 뒤집는다.
        reverse(_trace[0].begin(), _trace[0].end());

        stringstream ss;
        for (int i=0; i<_trace.size(); i++)
            ss << _trace[0][i] << " ";

        return ss.str();
    }

    vector<int64_t> _max_seq;
    Grid _trace;
};

class DynamicP
{
public:
    DynamicP(int size);

public:
    void print_to(ostream&);
    auto solve() -> Tracer;

private:
    void generate();

private:
    Grid _m;
    int _size;
};

DynamicP::DynamicP(int size)
{
    srand ((unsigned)time(NULL));

    _size = size;
    _m.resize(size);
    for (int i=0; i<_m.size(); i++)
        _m[i].resize(size, 0);

    generate();
}

void DynamicP::generate()
{
    _m[0][0] =  3;
    _m[1][0] = 28;
    _m[1][1] = 49;
    _m[2][0] =  4;
    _m[2][1] = 39;
    _m[2][2] =  7;

    /*
       for (int i=0; i<_m.size(); i++)
       for (int j=0; j<=i; j++)
       _m[i][j] = rand() % 50;
       */
}

void DynamicP::print_to(ostream& out)
{
    for (int i=0; i<_m.size(); i++)
    {
        for (int j=0; j<=i; j++)
            out << setfill('0') << setw(2) << _m[i][j] << " ";

        out << endl;
    }
}

Tracer DynamicP::solve()
{
    Tracer t(_m);

    for (auto i = int(_m.size())-2; i >= 0; i--)
    {
        auto copyy = t._trace;  

        for (int j=0; j<=i; j++)
        {
            auto a = t._max_seq[j];
            auto b = t._max_seq[j + 1];
            t._max_seq[j] = _m[i][j] + max<int64_t>(a, b);
            auto    trace = a > b ? t._trace[j] : t._trace[j + 1];
            trace.push_back(_m[i][j]);
            copyy[j] = trace;
        }

        t._trace = copyy;
    }

    return t;
}

int main()
{
    DynamicP DP(3);
    DP.print_to(cout);
    auto t = DP.solve();

    cout << "최대값 : " << t.max_value() << endl;
    cout << "경로  : " << t.max_trace();

    return 0;
}
