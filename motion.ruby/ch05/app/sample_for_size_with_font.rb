class SizeWithFontTest < UIView 
  def initWithFrame(frame)
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
    message = "SizeWithFont: 메소드로, 문자열을 출력하기 위해 필요한 크기 계산을 할 수 있다"
    actualFontSize = Pointer.new('f')
    system_font    = UIFont.systemFontOfSize(18)
    message.drawAtPoint(rect.origin,
               forWidth:rect.size.width, 
               withFont:system_font,
            minFontSize:6,
         actualFontSize:actualFontSize,
          lineBreakMode:UILineBreakModeWordWrap,
     baselineAdjustment:UIBaselineAdjustmentAlignCenters)

    puts "actualFontSize = #{actualFontSize[0]}"

    size = message.sizeWithFont(system_font)
    puts "sizeWithFont:의 실행결과"
    puts "size = #{size.width}, #{size.height}"

    size = message.sizeWithFont(system_font,
                      forWidth:rect.size.width,
                 lineBreakMode:UILineBreakModeTailTruncation)
    puts "sizeWithFont:forWidth:lineBreakMode의 실행결과"
    puts "size = #{size.width}, #{size.height}"

    size = message.sizeWithFont(system_font,
              constrainedToSize:rect.size,
                  lineBreakMode:UILineBreakModeCharacterWrap)
    puts "sizeWithFont:constrainedToSize:lineBreakMode의 실행결과"
    puts "size = #{size.width}, #{size.height}"

    size = message.sizeWithFont(system_font,
                    minFontSize:6,
                 actualFontSize:actualFontSize,
                       forWidth:rect.size.width,
                  lineBreakMode:UILineBreakModeWordWrap)
    puts "sizeWithFont:minFontSize:actualFontSize:forWidth:lineBreakMode의 실행결과"
    puts "size = #{size.width}, #{size.height}"
    puts "actualFontSize = #{actualFontSize[0]}"
  end
end

class SampleForSizeWithFont < UIViewController 
  def viewDidLoad
    super
    test1 = SizeWithFontTest.alloc.initWithFrame([[0, 10], [320, 66]])
    #test1.frame = [[0, 10], [320, 66]]
    self.view.addSubview(test1)
  end
end
