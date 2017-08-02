class X
{
public:
  X()
  {
    auto add = [*this] { };
  }
};

int main(int argc, char *argv[])
{

  return 0;
}
