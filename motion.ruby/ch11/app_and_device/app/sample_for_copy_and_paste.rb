
class SampleForCopyAndPaste < UIViewController
  def viewDidLoad
    super
  end

  def viewWillAppear animated
    super
    self.view.backgroundColor = UIColor.whiteColor

    image = UIImage.imageNamed("bug1.png")
    b1 = UIImageView.alloc.initWithIamge(image)
    b1.center = self.view.center
    self.view.addSubview(b1)

    image = UIImage.imageNamed("bug2.png")
    b2 = UIImageView.alloc.initWithIamge(image)
    b2.center = [200, 300]
    self.view.addSubview(b2)

    image = UIImage.imageNamed("bug3.png")
    b3 = UIImageView.alloc.initWithIamge(image)
    b3.center = [50, 80]
    self.view.addSubview(b3)
  end

  def canBecomeFirstResponder
    true
  end

  def touchEnded(touches, withEvent:event)
    touch = touch.anyObject
    if self.becomeFirstResponder and (touch.tabCount > 1)
      menu = UIMenuController.sharedMenuController
      @touch_point = touch.locationInView(self.view)
      min_rect = CGRect.new
      min_rect.origin = @touch_point
      menu.setTargetRect(min_rect, inView:self.view)
      menu.setMenuVisible(true, animated:true)
    end
  end

  def canPerformAction(action, withEvent:sender)
    # TODO
  end

  def imageContainsPoint(point)
  end

  def copy
  end

  def paste
  end

  def cut
  end

end
