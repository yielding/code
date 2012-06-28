class DrawStrings < UIView 
  def drawRect(rect)
    message = "UIKit용으로 추가된 NSString의 인스턴스 메소드를 사용해서 문자열 출력"
    system_font = UIFont.systemFontOfSize(UIFont.systemFontSize)
    # message.drawAtPoint([0, 0], withFont:system_font)
    message.drawInRect(rect, 
                       withFont:system_font, 
                       lineBreakMode:UILineBreakModeMiddleTruncation,
                       alignment:UITextAlignmentLeft)
  end
end

class SampleForDrawStrings < UIViewController
  def viewDidLoad
    super  
    strings  = DrawStrings.alloc.init 
    strings.frame = self.view.bounds
    strings.backgroundColor  = UIColor.whiteColor
    strings.autoresizingMask = 
      UIViewAutoresizingFlexibleWidth or UIViewAutoresizingFlexibleHeight
    self.view.addSubview(strings)
  end
end
