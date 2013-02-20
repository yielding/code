class TextAlignmentTest < UIView 
  attr_writer :text_alignment
  def initWithFrame(frame)
    if super
      @text_alignment = UITextAlignmentLeft
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
    message = 
      case @text_alignment
      when UITextAlignmentLeft
        "UITextAlignmentLeft"
      when UITextAlignmentCenter
        "UITextAlignmentCenter"
      when UITextAlignmentRight
        "UITextAlignmentRight"
      else
        "None"
      end

    system_font = UIFont.systemFontOfSize(18)
    message.drawInRect(rect,
      withFont:system_font,
      lineBreakMode:UILineBreakModeWordWrap,
      alignment:@text_alignment)
  end
end

class SampleForTextAlignment < UIViewController
  def viewDidLoad
    super
    t1 = TextAlignmentTest.alloc.initWithFrame(CGRectMake(0, 10, 320, 40))
    t1.text_alignment = UITextAlignmentLeft
    view.addSubview(t1)
    t2 = TextAlignmentTest.alloc.initWithFrame([[0, 70], [320, 40]])
    t2.text_alignment = UITextAlignmentCenter
    view.addSubview(t2)
    t3 = TextAlignmentTest.alloc.initWithFrame([[0, 130], [320, 40]])
    t3.text_alignment = UITextAlignmentRight
    view.addSubview(t3)
  end
end
