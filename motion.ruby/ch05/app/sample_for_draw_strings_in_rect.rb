class DrawStringsInRect < UIView 
  def drawRect(rect)
    message = "UIKit용으로 추가된 NSString의 인스턴스 메소드를 사용해 문자열을 출력."
    system_font = UIFont.systemFontOfSize(UIFont.systemFontSize)
    message.drawInRect(rect, withFont:system_font)
  end
end

class SampleForDrawStringsInRect < UIViewController 
  def viewDidLoad
    super
    strs = DrawStringsInRect.alloc.init 
    strs.frame = self.view.bounds
    strs.backgroundColor = UIColor.whiteColor
    strs.autoresizingMask =
      UIViewAutoresizingFlexibleWidth or 
      UIViewAutoresizingFlexibleHeight
    view.addSubview(strs)
  end
end
