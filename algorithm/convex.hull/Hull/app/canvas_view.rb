class CanvasView < UIView
  def initWithFrame(rect)
    if super
      @hull = JarvisMarch.new
      @points = []
      @sz = 0
      @poly_mode = false
    end

    self
  end

  # commands
  def clearCanvas
    @points = []
    @sz = 0
    @poly_mode = false

    setNeedsDisplay
  end

  def findHull
    @poly_mode = true
    @sz     = @hull.compute_hull(@points.clone)
    @points = @hull.p

    setNeedsDisplay
  end


  def drawRect(rect)
    UIColor.blackColor.set
    UIBezierPath.bezierPathWithRect(rect).fill

    if @poly_mode
      pts = @points.slice(0, @sz).map { |p| [p.x, p.y] }
      drawPoly pts
    end

    #@points.slice(0, @sz).each { |pt| drawPoint pt }
    @points.each { |pt| drawPoint pt }
  end

  def drawLine(p0, p1)
    path = UIBezierPath.new
    path.lineWidth = 2
    path.moveToPoint p0
    path.addLineToPoint p1
    UIColor.orangeColor.setStroke
    line.stroke
  end

  def drawPoint p, size=4
    x, y = p.x, p.y
    p0, p1 = [x - size, y - size], [x + size, y - size]
    p2, p3 = [x + size, y + size], [x - size, y + size]
    drawPoly [p0, p1, p2, p3]
  end

  def drawPoly points
    pl = UIBezierPath.new
    pl.lineWidth = 2
    points.each.with_index { |p, i| 
      i == 0 ? pl.moveToPoint(p) : pl.addLineToPoint(p) 
    }
    pl.closePath

    UIColor.orangeColor.setFill
    pl.fill
    UIColor.yellowColor.setStroke
    pl.stroke
  end

  def touchesBegan(touches, withEvent:e)
    touch = e.touchesForView(self).anyObject
    point = touch.locationInView(self)

    @points << Point.new(point[0], point[1])
    @sz = @points.size
    setNeedsDisplay
  end

  def touchesMoved(touches, withEvent:e)
  end

  def touchesEnded(touches, withEvent:e)
    @prev_point = nil
  end
end
