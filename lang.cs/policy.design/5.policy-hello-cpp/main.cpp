#include <iostream>
#include <string>

using namespace std;


template <typename OutputPolicy, typename LanguagePolicy>
class HelloWorld : private OutputPolicy, private LanguagePolicy
{
    using OutputPolicy::print;
    using LanguagePolicy::message;

public:

    void run() const
    {
        // Two policy methods
        // !! 두 개의 policy는 서로 상관이 없다. -> orthogonal -> n x m이 n + m 으로 
        print(message());
    }
};


class OutputPolicyWriteToCout
{
protected:
    template<typename MessageType>
    void print(MessageType const &message) const
    {
        std::cout << message << std::endl;
    }
};

class OutputPolicyWriteToFile
{
protected:
    template<typename MessageType>
    void print(MessageType const &message) const
    {
      // ..
    }
};


class LanguagePolicyEnglish
{
protected:
    std::string message() const
    {
        return "Hello, World!";
    }
};

class LanguagePolicyGerman
{
protected:
    std::string message() const
    {
        return "Hallo Welt!";
    }
};

int main()
{
    using HelloWorldEnglish = HelloWorld<OutputPolicyWriteToCout, LanguagePolicyEnglish>;
    HelloWorldEnglish h1;
    h1.run(); // prints "Hello, World!"

    using HelloWorldGerman = HelloWorld<OutputPolicyWriteToCout, LanguagePolicyGerman>; 
    HelloWorldGerman h2;
    h2.run(); // prints "Hallo Welt!"
}
