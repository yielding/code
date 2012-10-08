class SampleForBatteryMonitor < UIViewController
  def viewDidLoad
    super
    @tv = UITextView.new
    @tv.editable = false
    @tv.frame    = self.view.bounds
    @tv.autoresizingMask = 
      UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleHeight
    @tv.backgroundColor = UIColor.blackColor
    @tv.textColor = UIColor.whiteColor
    @tv.font = UIFont.systemFontOfSize(16)
    self.view.addSubview(@tv)

    btn = UIBarButtonItem.alloc.initWithBarButtonSystemItem(UIBarButtonSystemItemRefresh, 
                                                            target:self,
                                                            action:'refreshDidPush')
    self.navigationItem.rightBarButtonItem = btn

    UIDevice.currentDevice.batteryMonitoringEnabled = true
    self.refreshDidPush

    noti = NSNotificationCenter.defaultCenter
    noti.addObserver(self,
                     selector:'refreshDidPush',
                     name:UIDeviceBatteryLevelDidChangeNotification,
                     object:nil)
    noti.addObserver(self,
                     selector:'refreshDidPush',
                     name:UIDeviceBatteryStateDidChangeNotification,
                     object:nil)
  end

  def refreshDidPush
    device = UIDevice.currentDevice
    s = self.batteryStateToString(device.batteryState)
    text  = "batteryState: #{s}\n"
    text += "batteryLevel: #{device.batteryLevel}"
    @tv.text = text
  end

  def batteryStateToString state
    case state
    when UIDeviceBatteryStateUnplugged; "UIDeviceBatteryStateUnplugged"
    when UIDeviceBatteryStateCharging;  "UIDeviceBatteryStateCharging"
    when UIDeviceBatteryStateFull;      "UIDeviceBatteryStateFull"
    else
      "UIDeviceBatteryStateUnknown"
    end
  end

end
