roots1 a b c = 
  ((-b + sqrt(b*b - 4*a*c)) / (2*a),
   (-b - sqrt(b*b - 4*a*c)) / (2*a))

roots2 a b c = 
  let dst = sqrt(b*b - 4*a*c)
      da  = 2*a 
  in ((-b + dst) / da, (-b - dst) / da)