import re

p = re.compile('section{([^}]*)}', re.VERBOSE)
print p.sub(r'subsection{\1}','section{First} section{second}')
# 결과 : subsection{First} subsection{second}

# replacement 부분에 함수가 올 수 있다. 이 경우 매우 강력한 문자 처리가 가능하다.
# 함수가 오면 패턴과 일치할 때마다 MatchObject 인스턴스를 함수에 전달한다.
# 함수에서 이 인스턴스를 사용하여 가공한 후, 원하는 문자열을 반환한다. 


p = re.compile('section{ (?P<name> [^}]* ) }', re.VERBOSE)
print p.sub(r'subsection{\1}','section{First}')
# 'subsection{First}'
print p.sub(r'subsection{\g<1>}','section{First}')
# 'subsection{First}'
print p.sub(r'subsection{\g<name>}','section{First}')
# 'subsection{First}'
