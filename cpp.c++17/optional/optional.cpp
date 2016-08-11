#include <string>
#include <iostream>
#include <optional>

using namespace std::string_literals;
using namespace std;

// optional can be used as the return type of a factory that may fail
std::optional<std::string> create(bool b) 
{
	if (b)
		return "Godzilla"s;
  else
    return {};
}

int main()
{
	cout << "create(false) returned "
		<< create(false).value_or("empty") << '\n';

	// optional-returning factory functions are usable as conditions of while and if
	if (auto str = create(true)) {
		std::cout << "create(true) returned " << *str << '\n';
	}
}
