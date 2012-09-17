class SampleForSetData < UIViewController
  def viewDidLoad
    super

    @date_picker = UIDatePicker.new
    @date_picker.date = NSDate.dateWithTimeIntervalSinceNow(-3600*24)
    self.view.addSubview(@date_picker)

    btn = UIButton.buttonWithType(UIButtonTypeRoundedRect)
    btn.setTitle("오늘로 돌아가기", forState:UIControlStateNormal)
    btn.sizeToFit

    new_point = self.view.center
    new_point.y += 50
    btn.center = new_point

    btn.addTarget(self, action:"buttonDidPush",
                  forControlEvents:UIControlEventTouchUpInside)
    self.view.addSubview(btn)
  end

  def buttonDidPush
    @date_picker.setDate(NSDate.date)
  end
end
