class TouchableLabel < UILabel
  def touchesBegan(touches, withEvent:event)
    alert = UIAlertView.alloc.initWithTitle(nil,
                   message:"I'm a label!",
                   delegate:nil,
                   cancelButtonTitle:nil,
                   otherButtonTitles:"OK",
                   nil)
    alert.show
  end
end

class SampleForTouchesLabel < UIViewController
  def viewDidLoad
    super
    view.backgroundColor = UIColor.whiteColor
    label = TouchableLabel.alloc.init 
    label.frame = [[60, 100], [200, 50]]
    label.text  = "Touch me!"
    label.textAlignment = UITextAlignmentCenter
    label.userInteractionEnabled = true
    view.addSubview(label)
  end

  def touchesBegan(touches, withEvent:event)
    alert = UIAlertView.alloc.initWithTitle(nil,
              message:"I'm a view controller!",
              delegate:nil,
              cancelButtonTitle:nil,
              otherButtonTitles:"OK",
              nil)
    alert.show
  end
end
