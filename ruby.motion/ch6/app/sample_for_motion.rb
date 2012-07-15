class LabelForMotion < UILabel
  def canBecomeFirstResponder
    true
  end
end

class SampleForMotion < UIViewController
  def viewDidLoad
    super
    l = LabelForMotion.alloc.init 
    l.frame = self.view.bounds
    l.autoresizingMask = UIViewAutoresizingFlexibleWidth or UIViewAutoresizingFlexibleHeight
    l.textAlignment    = UITextAlignmentCenter
    l.text = "Shake me"
    self.view.addSubview(l)
    l.becomeFirstResponder
  end

  def motionBegan(motion, withEvent:event)
    puts "motion began"
  end

  def motionEnded(motion, withEvent:event)
    puts "motion ended"
    alert = UIAlertView.alloc.initWithTitle(nil,
              message:"Earth quake!!",
              delegate:nil, 
              cancelButtonTitle:nil, 
              otherButtonTitles:"OK", nil)
    alert.show
  end

  def motionCancelled(motion, withEvent:event)
    puts "motion cancelled"
  end
end
