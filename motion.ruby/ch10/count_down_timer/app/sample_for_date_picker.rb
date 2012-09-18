class SampleForDatePicker < UIViewController
  def viewDidLoad
    super

    @date_picker = UIDatePicker.new
    view.addSubview(@date_picker)

    @date_picker.addTarget(self, action:"pickerDidChange:", 
                                 forControlEvents:UIControlEventValueChanged)

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
    formatter = NSDateFormatter.new
    formatter.setDateFormat("yyyy/MM/dd HH:mm")
    date_str = formatter.stringFromDate(@date_picker.date)
    alert = UIAlertView.new
    alert.message = date_str
    alert.addButtonWithTitle("OK")
    alert.show
  end

  def pickerDidChange(picker)
    if picker.class == UIDatePicker
      NSLog("%s", picker.date.description)
    end
  end
end
