#ifndef CPPEXAMPLE_H
#define CPPEXAMPLE_H

#include <string>
#include <vector>

class CppExample 
{
public:
	CppExample();
	CppExample(const CppExample& foo);
	CppExample(const char* title, int flag);
	virtual ~CppExample();
	
	std::string title() const;
  void        title(std::string title);
	//void        title(const char* title);
	
	int         flag() const;
	void        flag(int value);
	
	static int countOfCppExamples();
private:
	std::string _title;
	int         _flag;
	
  std::vector<int> m_ints;
};

#endif
