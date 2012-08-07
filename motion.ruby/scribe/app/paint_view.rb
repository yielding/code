class PaintView < UIView 
   #def initWithFrame(frame) if super
      #@hue = 0.5
    #end
    #self
  #end

  #def touchesMoved(touches, withEvent:event)
    #@touch = touches.anyObject
    #self.setNeedsDisplay()
  #end

  #def drawRect(rect)
    #context = UIGraphicsGetCurrentContext()

    #color = UIColor.colorWithHue(@hue, saturation:0.7, brightness:1.0, alpha:1.0)

    #CGContextSetStrokeColorWithColor(context, color.CGColor)
    #CGContextSetLineCap(context, KCGLineCapRound)
    #CGContextSetLineWidth(context, 15)
    
    #unless @touch.nil?
      #lp = @touch.previousLocationInView(self)
      #np = @touch.locationInView(self)

      #CGContextMoveToPoint(context, lp.x, lp.y)
      #CGContextAddLineToPoint(context, np.x, np.y)
      #CGContextStrokePath(context)
    #end
  #end

  def initWithFrame(frame)
    if super 
      @hue = 0.0
      self.init_cache_context(frame.size)
    end
    self
  end

  def init_cache_context(size)
    bitmap_bytes_per_row = size.width * 4
    bitmap_byte_count = bitmap_bytes_per_row * size.height

    cache_bitmap = Pointer.new(:char, bitmap_byte_count)
    @cached_context = CGBitmapContextCreate(cache_bitmap, 
                                            size.width, 
                                            size.height,
                                            8,
                                            bitmap_bytes_per_row,
                                            CGColorSpaceCreateDeviceRGB(),
                                            KCGImageAlphaNoneSkipFirst)
    true
  end

  def touchesMoved(touches, withEvent:event)
    @touch = touches.anyObject
    self.draw_to_cache(@touch)
  end

  def draw_to_cache(touch)
    @hue += 0.005
    @hue = 0 if @hue > 1.0

    color = UIColor.colorWithHue(@hue, 
                                 saturation:0.7, 
                                 brightness:1.0, 
                                 alpha:1.0)
    CGContextSetStrokeColorWithColor(@cached_context, color.CGColor)
    CGContextSetLineCap(@cached_context, KCGLineCapRound)
    CGContextSetLineWidth(@cached_context, 15)

    lp = @touch.previousLocationInView(self)
    np = @touch.locationInView(self)
   
    CGContextMoveToPoint(@cached_context, lp.x, lp.y)
    CGContextAddLineToPoint(@cached_context, np.x, np.y)
    CGContextStrokePath(@cached_context)
    dp1 = CGRectMake(lp.x - 10, lp.y - 10, 20, 20)
    dp2 = CGRectMake(np.x - 10, np.y - 10, 20, 20)
    self.setNeedsDisplayInRect(dp1, dp2)
  end

  def drawRect(rect)
    ctx = UIGraphicsGetCurrentContext()
    cache_img = CGBitmapContextCreateImage(@cached_context)
    CGContextDrawImage(ctx, self.bounds, cache_img)
  end

end
