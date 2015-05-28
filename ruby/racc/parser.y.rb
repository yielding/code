class CalcParser
prechigh
  nonassoc '++'
  left     '*' '/'
  left     '+' '-'
  right    '='
preclow
rule
 target: exp { print val[0] }

 exp : exp '+' exp
     | exp '*' exp
     | '(' exp ')'
     | NUMber
end
