class SampleForAnimationCurve < UIViewController

  @@animation_curve = UIViewAnimationCurveEaseInOut

  def viewDidLoad
    super
    self.view.backgroundColor = UIColor.blackColor
    
    image = UIImage.imageNamed("star.png")
    @star = UIImageView.alloc.initWithImage(image)
    @star.center = [self.view.center.x, -100]
    self.view.addSubview(@star)

    @label = UILabel.alloc.init 
    @label.frame = [[0, self.view.bounds.size.height - 20], [320, 20]]
    @label.autoresizingMask =
      UIViewAutoresizingFlexibleTopMargin or 
      UIViewAutoresizingFlexibleBottomMargin 
    @label.textAlignment = UITextAlignmentCenter 
    @label.text = "UIViewAnimationCurveEaseInOut"
    self.view.addSubview(@label)
  end

  def touchesEnded(touches, withEvent:event)
    @star.center = [self.view.center.x, -100]
    @star.alpha  = 1.0

    UIView.beginAnimations(nil, context:nil)
    UIView.setAnimationCurve(@@animation_curve)
    UIView.setAnimationDuration(2.0)
    @star.center = [self.view.center.x, 300]
    @star.alpha  = 0.0
    UIView.commitAnimations 
    
    @label.text = 
      case @@animation_curve
      when UIViewAnimationCurveEaseInOut; "UIViewAnimationCurveEaseInOut"
      when UIViewAnimationCurveEaseIn;    "UIViewAnimationCurveEaseIn"
      when UIViewAnimationCurveEaseOut;   "UIViewAnimationCurveEaseOut"
      when UIViewAnimationCurveLinear;    "UIViewAnimationCurveLinear"
      else 
        "-"
      end

    @@animation_curve += 1
    if (UIViewAnimationCurveLinear < @@animation_curve)
      @@animation_curve = UIViewAnimationCurveEaseInOut 
    end
  end
     
end
