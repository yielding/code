class SampleForTransition < UIViewController 
  KTagViewForTransitionsTest = 1
  @@is_front = true

  def viewDidLoad
    super
    @transition = UIViewAnimationTransitionFlipFromLeft
    view.addSubview(next_view)
  end

  def touchesEnded(touches, withEvent:event)
    unless UIView.areAnimationsEnabled
      self.nextResponder.touchesEnded(touches, withEvent:event)
      return
    end

    nv = self.next_view 
    UIView.beginAnimations(nil, context:nil)
    UIView.setAnimationDelegate(self)
    UIView.setAnimationDidStopSelector("animationDidStop")
    UIView.setAnimationDuration(1.0)
    UIView.setAnimationTransition(@transition, forView:view, cache:true)
    view.viewWithTag(KTagViewForTransitionsTest)
        .removeFromSuperview 
    view.addSubview(nv)
    UIView.commitAnimations
    UIView.setAnimationsEnabled(false)

    @transition += 1
    if UIViewAnimationTransitionCurlDown < @transition
      @transition = UIViewAnimationTransitionFlipFromLeft
    end
  end

  def next_view
    image_name = @@is_front ? "dog.jpg" : "town.jpg"
    @@is_front = (@@is_front != true)
    image = UIImage.imageNamed(image_name)

    view = UIImageView.alloc.initWithImage(image)
    view.tag = KTagViewForTransitionsTest
    view.frame = self.view.bounds
    view.autoresizingMask = 
      UIViewAutoresizingFlexibleWidth or
      UIViewAutoresizingFlexibleHeight
    view.contentMode = UIViewContentModeScaleAspectFill
    view
  end

  def animationDidStop
    UIView.setAnimationsEnabled(true)
    puts "animationDidStop"
  end

end
