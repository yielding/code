# Data Hiding (Encapsulation)
  1. class
     - public
     - protected
     - private

  2. object identification

# Inheritance
  1. code reuse (X)
     codee resue -> composition
     relationship: is-a, has-a

  2. c++ multiple inheritance
     class Car: public Wagan, protected IDisposable, protected IMovable

  3. c#, java single inheritance
     클래스는 한개만 상속을 받을 수 있다. 
     하지만 1개이상의 interface를 구현할 수 있다.
     c#   => class Car: Wagan, IDisposable, IMovable, IDeletable
     java => class Car extends Wagan implement IDisposable

# Polymorphism
  하나의 함수가 인자의 타입에 따라서 다르게 동작하는 것.                         
  이때 인자의 타입은 상속관계가 있는경우

interface Image {
    int find_begin_mark();
    int find_end_mark();
}
public class JPEG {
    int find_begin_mark()
    {
    }

    int find_end_mark()
    {
    }
}

public class PNG {
    public int find_begin_mark()
    {
    }

    public int find_end_mark()
    {
    }
}

public class BMP {
    public int find_begin_mark()
    {
    }

    public int find_end_mark()
    {
    }
}

--- lib
void very_hard_algorithm(ref A a)
{
    int beg = a.find_begin_mark();
    int end = a.find_end_mark();
}

public interface Stack {
    void pop();
    int  top();
    void push(int a);
    bool is_empty();
}

void stack_operation(ref Stack a)
{
    a.push(1);
    a.push(2);
    a.push(3);

    while (!a.is_empty())
    {
        print a.top();
        a.pop();
    }
}


--- main
public class D : A {
    public override void print() {
        print "I'm D";
    }
}
print(new A());
print(new B());
print(new C());
print(new D());

# message passing

# 중복

# 테스트
  design by test

