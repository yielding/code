class SampleForDatePickerMode < UIViewController
  def viewDidLoad
    super

    @date_picker = UIDatePicker.new
    view.addSubview @date_picker

    button = UIButton.buttonWithType(UIButtonTypeRoundedRect)
    button.setTitle("모드전환", forState:UIControlStateNormal)
    button.sizeToFit

    new_point    = self.view.center
    new_point.y += 50
    button.center= new_point
    button.addTarget(self, action:"buttonDidPush",
                           forControlEvents:UIControlEventTouchUpInside)

    self.view.addSubview(button)
  end

  def buttonDidPush
    @date_picker.datePickerMode += 1
    if UIDatePickerModeCountDownTimer < @date_picker.datePickerMode
      @date_picker.datePickerMode = UIDatePickerModeTime
    end
  end
end
