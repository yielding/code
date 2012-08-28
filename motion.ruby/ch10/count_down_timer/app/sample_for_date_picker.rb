class SampleForDatePicker < UIViewController
  def viewDidLoad
    super

    @date_picker = UIDatePicker.new
    view.addSubview(@date_picker)

    @date_picker.addTarget(self, action:"pickerDidChange:", forControlEvents:UIControlEventValueChanged)

    btn = UIButton.buttonWithType(UIButtonTypeRoundedRect)
    btn.setTitle("일자표시", forState:UIControlStateNormal)
    btn.sizeToFit

    pt = self.view.center
    pt.y += 50
    btn.center = pt
    btn.addTarget(self, action:"buttonDidPush", 
                 forControlEvents:UIControlEventTouchUpInside)
    self.view.addSubview(btn)
  end

  def buttonDidPush
    formatter = NSDateFormatter
  end

  def pickerDidChange(picker)
  end
end
