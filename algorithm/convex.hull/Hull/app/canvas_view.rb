class CanvasView < UIView
  SZ = 7
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
      drawPoly pts, UIColor.blueColor
    end

    @points.each { |pt| drawPoint pt }
  end

  def drawPoint p, size=SZ
    x, y = p.x, p.y
    p0, p1 = [x - size, y - size], [x + size, y - size]
    p2, p3 = [x + size, y + size], [x - size, y + size]
    drawPoly [p0, p1, p2, p3], UIColor.orangeColor
  end

  def drawPoly(points, fc=UIColor.orangeColor, sc=UIColor.yellowColor)
    pl = UIBezierPath.new
    pl.lineWidth = 2
    points.each.with_index do |p, i| 
      i == 0 ? pl.moveToPoint(p) : pl.addLineToPoint(p) 
    end
    pl.closePath

    fc.setFill;   pl.fill
    sc.setStroke; pl.stroke
  end

  def touchesBegan(touches, withEvent:e)
    touch = e.touchesForView(self).anyObject
    pt    = touch.locationInView(self)

    @selected = @points.find_index do |p| 
      p.x.between?(pt[0]-SZ, pt[0]+SZ) and
      p.y.between?(pt[1]-SZ, pt[1]+SZ) 
    end

    if @selected.nil?
      @points << Point.new(pt[0], pt[1])
      @sz = @points.size
      setNeedsDisplay
    end
  end

  def touchesMoved(touches, withEvent:e)
    if not @selected.nil?
      touch = e.touchesForView(self).anyObject
      pt    = touch.locationInView(self)
      @points[@selected].x = pt[0]
      @points[@selected].y = pt[1]
      setNeedsDisplay
    end
  end

  def touchesEnded(touches, withEvent:e)
    @selected = nil
  end
end
