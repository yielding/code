#!/usr/bin/env ruby

def spiral(xDim, yDim)
   sx = xDim / 2
   sy = yDim / 2

   cx = cy = 0
   direction = distance = 1

   yield(cx,cy)
   while(cx.abs <= sx || cy.abs <= sy)
      distance.times { cx += direction; yield(cx,cy) if(cx.abs <= sx && cy.abs <= sy); } 
      distance.times { cy += direction; yield(cx,cy) if(cx.abs <= sx && cy.abs <= sy); } 
      distance += 1
      direction *= -1
   end
end

spiral(5,3) { |x,y| print "(#{x},#{y})," }
