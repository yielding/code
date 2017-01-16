#!/usr/bin/env ruby

<<<<<<< HEAD
def positions_of(x, in_:)
    in_.zip([*0..in_.length-1])
      .select { |p| p[0] == x }
      .map    { |p| p[1] }
end

p positions_of(2, in_: [1, 2, 3, 4, 5, 2])
=======
def positions_of(x, inside:)
  xs = inside

  xs.zip([*0..xs.length-1])
    .select { |p| p[0] == x }
    .map    { |p| p[1] }
end

p positions_of(2, inside: [1, 2, 3, 4, 5, 2])
>>>>>>> 6ebc614a2fa1e62f72f686aa6699746856199338
