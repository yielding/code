class MinFontSizeTest < UIView 
  def init 
    if super
      self.backgroundColor  = UIColor.whiteColor
      self.autoresizingMask = 
        UIViewAutoresizingFlexibleLeftMargin or 
        UIViewAutoresizingFlexibleRightMargin or 
        UIViewAutoresizingFlexibleTopMargin or 
        UIViewAutoresizingFlexibleBottomMargin 
    end 
    self 
  end

  def drawRect(rect)
    message = "전부 들어가지 않는 경우, minFontSize를 최소화 해서 폰트를 자동 축소"
    system_font = UIFont.systemFontOfSize 36
    ptr = Pointer.new("f")
    message.drawAtPoint(rect.origin,
                        forWidth:rect.size.width,
                        withFont:system_font,
                        minFontSize:6, 
                        actualFontSize:ptr,
                        lineBreakMode:UILineBreakModeWordWrap,
                        baselineAdjustment:UIBaselineAdjustmentAlignCenters)
    puts "actualFontSize = #{ptr[0]}"
  end
end

class SampleForMinFontSize < UIViewController
  def viewDidLoad
    super
    test = MinFontSizeTest.alloc.init 
    test.frame = [[0, 10], [320, 40]]
    self.view.addSubview(test)
  end

end
