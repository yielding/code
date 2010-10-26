#!/opt/local/bin/osxvm

# Vectorized by Steve Dekorte

printSet := method (
  bailout := 16
  max_iterations := 1000

  cr := Vector clone
  ci := Vector clone

  i := 0
  for (y, -39, 39,
    for (x, -39, 39,
      cr atPut(i, y/40.0 - 0.5)
      ci atPut(i, (x/40.0))
      i = i + 1
    )
  )

  size := cr size

  zi   := Vector clone setSize(size)
  zr   := Vector clone setSize(size) 
  zr2  := Vector clone setSize(size)

  zi2  := Vector clone setSize(size)
  temp := Vector clone setSize(size)

  for (i, 1, max_iterations,
    temp copy(zr) *= zi

    zr2 copy(zr) square
    zi2 copy(zi) square

    zr copy(zr2) -= zi2 
    zr += cr
    zi copy(temp) *= 2
    zi += ci
  )

  result := zi2 + zr2

  i := 0
  for (y, -39, 39,
    writeln
    for (x, -39, 39,
      r := result at(i) 
      write(if( r > 0, "*", " "))
      i = i + 1
    )
  )
)

writeln("\nIo Elapsed " .. Date secondsToRun(printSet))

