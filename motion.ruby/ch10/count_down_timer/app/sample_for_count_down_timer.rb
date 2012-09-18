class SampleForCountDownTimer < UIViewController
  def viewDidLoad
    super
    @date_picker = UIDatePicker.new
    @date_picker.datePickerMode    = UIDatePickerModeCountDownTimer
    @date_picker.countDownDuration = 60 * 5
    self.view.addSubview(@date_picker)

    @timer = NSTimer.timerWithTimeInterval(60.0,
                     target:self,
                     selector:"timerFiredMethod:",
                     userInfo:nil,
                     repeats:true)

    run_loop = NSRunLoop.currentRunLoop
    run_loop.addTimer(@timer, forMode:NSDefaultRunLoopMode)
  end

  def viewWillDisappear animated
    super
    @timer.invalidate if @timer.valid?
  end

  def timerFiredMethod theTimer
    now = @date_picker.countDownDuration
          @date_picker.countDownDuration = now - 60.0 if now > 0.0
  end
end
