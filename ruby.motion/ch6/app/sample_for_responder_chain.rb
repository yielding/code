class TouchableLabelForResponder < UILabel 
  def init
    if super
      userInteractionEnabled = true
      autoresizingMask = 
        UIViewAutoresizingFlexibleWidth      or UIViewAutoresizingFlexibleHeight or 
        UIViewAutoresizingFlexibleLeftMargin or UIViewAutoresizingFlexibleRightMargin or 
        UIViewAutoresizingFlexibleTopMargin  or UIViewAutoresizingFlexibleBottomMargin
    end
    self
  end

  def touchesBegan(touches, withEvent:event)
    puts self.text
    self.nextResponder.touchesBegan(touches, withEvent:event)
  end
end

class SampleForResponderChain < UIViewController
  def viewDidLoad
    super

    grandpa = TouchableLabelForResponder.alloc.init 
    grandpa.frame = CGRectInset(self.view.bounds, 40, 20)
    grandpa.text  = "A"
    grandpa.backgroundColor = UIColor.darkGrayColor
    view.addSubview(grandpa)

    father = TouchableLabelForResponder.alloc.init 
    father.frame = CGRectInset(grandpa.bounds, 40, 20)
    father.text  = "B"
    father.backgroundColor = UIColor.grayColor
    grandpa.addSubview(father)

    me = TouchableLabelForResponder.alloc.init 
    me.frame = CGRectInset(father.bounds, 40, 20)
    me.text  = "C"
    me.backgroundColor = UIColor.whiteColor
    father.addSubview(me)
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
