class SampleForPinch < UIViewController 
  def viewDidLoad
    super
    self.view.backgroundColor = UIColor.blackColor
    self.view.multipleTouchEnabled = true

    img = UIImage.imageNamed("dog.jpg")
    @img_view = UIImageView.alloc.initWithImage(img)
    @img_view.center = self.view.center
    @img_view.contentMode = UIViewContentModeScaleAspectFill
    @img_view.autoresizingMask = 
      UIViewAutoresizingFlexibleLeftMargin or 
      UIViewAutoresizingFlexibleRightMargin or 
      UIViewAutoresizingFlexibleTopMargin or 
      UIViewAutoresizingFlexibleBottomMargin
    self.view.addSubview(@img_view)
  end

  def touchesMoved(touches, withEvent:event)
    return unless touches.count == 2

    two_fingers = touches.allObjects
    t1 = two_fingers[0]
    t2 = two_fingers[1]

    p1 = t1.previousLocationInView(self.view)
    p2 = t2.previousLocationInView(self.view)
    
    n1 = t1.locationInView(self.view)
    n2 = t2.locationInView(self.view)

    p_dist = distance_between(p1, n: p2)
    c_dist = distance_between(n1, n: n2)

    scale  = 1.0
    scale -= (p_dist / c_dist) / 300.0 if (p_dist > c_dist)
    scale += (c_dist / p_dist) / 300.0 if (c_dist > p_dist)

    new_transform = CGAffineTransformScale(@img_view.transform, scale, scale)
    @img_view.transform = new_transform
    @img_view.center = self.view.center
  end

  def distance_between(pa, n: pb)
    dx = (pb.x - pa.x).abs
    dy = (pb.y - pa.y).abs

    Math.sqrt(dx*dx + dy*dy)
  end
end
