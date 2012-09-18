class SampleForMinuteInterval < UIViewController
  def viewDidLoad
    super

    @date_picker = UIDatePicker.new
    @date_picker.minuteInterval = 10
    self.view.addSubview(@date_picker)
  end
end
