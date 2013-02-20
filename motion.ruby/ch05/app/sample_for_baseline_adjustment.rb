class BaselineAdjustmentTest < UIView 
  attr_writer :baseline_adjustment

  def initWithFrame(frame)
    if super
      @baseline_adjustment = -1
      self.backgroundColor = UIColor.whiteColor
      self.autoresizingMask = 
          UIViewAutoresizingFlexibleLeftMargin or
          UIViewAutoresizingFlexibleRightMargin or
          UIViewAutoresizingFlexibleTopMargin or 
          UIViewAutoresizingFlexibleBottomMargin
      end
    self
  end

  def drawRect(rect)
    message   = "UIKit용으로 추가된 NSString의 인스턴스 메소드를 사용해서 문자열 출력"
    sys_font  = UIFont.systemFontOfSize(36)
    font_size = (@baseline_adjustment == -1) ? 30 : 10

    message.drawAtPoint(rect.origin,
                        forWidth:rect.size.width,
                        withFont:sys_font,
                        fontSize:font_size,
                        lineBreakMode:UILineBreakModeWordWrap, 
                        baselineAdjustment:@baseline_adjustment)
  end
end

class SampleForBaselineAdjustment < UIViewController
  def viewDidLoad
    super  
    test0 = BaselineAdjustmentTest.alloc.initWithFrame [[0, 10], [320, 40]]
    test0.baseline_adjustment = -1
    self.view.addSubview(test0)

    test1 = BaselineAdjustmentTest.alloc.initWithFrame [[0, 70], [320, 40]]
    test1.baseline_adjustment = UIBaselineAdjustmentAlignBaselines
    self.view.addSubview(test1)

    test2 = BaselineAdjustmentTest.alloc.initWithFrame [[0, 130], [320, 40]] 
    test2.baseline_adjustment = UIBaselineAdjustmentAlignCenters
    self.view.addSubview(test2)

    test3 = BaselineAdjustmentTest.alloc.initWithFrame [[0, 190], [320, 40]]
    test3.baseline_adjustment = UIBaselineAdjustmentNone
    self.view.addSubview(test3)
  end
end
