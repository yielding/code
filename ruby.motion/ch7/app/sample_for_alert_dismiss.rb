class AlertDismiss < UIViewController
  def viewDidAppear(animated)
    super(animated)
    alert = UIAlertView.alloc.init 
    alert.message = "This message will disappear in 3 secs"
    alert.addButtonWithTitle("Cancel")
    alert.cancelButtonIndex = 0
    alert.show

    self.performSelector(:"dismissAlert:", 
                         withObject: alert, 
                         afterDealy: 3.0)
  end

  def dismissAlert alert 
    puts "dismiss Alert"

    if alert.visible
      alert.dismissWithClickedButtonIndex(alert.cancelButtonIndex, 
                                          animated:true) 
    end
  end
end
