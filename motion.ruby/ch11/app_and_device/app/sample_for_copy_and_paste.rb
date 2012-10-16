class SampleForCopyAndPaste < UIViewController
  def viewWillAppear animated
    super
    self.view.backgroundColor = UIColor.whiteColor

    image = UIImage.imageNamed("bug1.png")
    b1 = UIImageView.alloc.initWithImage(image)
    b1.center = self.view.center
    self.view.addSubview(b1)

    image = UIImage.imageNamed("bug2.png")
    b2 = UIImageView.alloc.initWithImage(image)
    b2.center = [200, 300]
    self.view.addSubview(b2)

    image = UIImage.imageNamed("bug3.png")
    b3 = UIImageView.alloc.initWithImage(image)
    b3.center = [50, 80]
    self.view.addSubview(b3)
  end

  def canBecomeFirstResponder
    true
  end

  def touchesEnded(touches, withEvent:event)
    touch = touches.anyObject
    if self.becomeFirstResponder and (touch.tapCount > 1)
      menu = UIMenuController.sharedMenuController
      @touch_point = touch.locationInView(self.view)
      min_rect = CGRect.new
      min_rect.origin = @touch_point
      menu.setTargetRect(min_rect, inView:self.view)
      menu.setMenuVisible(true, animated:true)
    end
  end

  def canPerformAction(action, withSender:sender)
    res = case action
          when :"copy:"
            puts "copy"
            true if self.imageContainsPoint(@touch_point)
          when :"cut:"   
            puts "cut"
            true if self.imageContainsPoint(@touch_point)
          when :"paste:"
            puts "paste"
            true unless UIPasteboard.generalPasteboard.image.nil?
          else
            false
          end
    res
  end

  def imageContainsPoint(point)
    view.subviews.each { |view|  
      if CGRectContainsPoint(view.frame, point)
        return view if view.class == UIImageView
      end
    }

    nil
  end

  def copy(sender)
    iv = self.imageContainsPoint(@touch_point)
    if iv
      puts "copy"
      UIPasteboard.generalPasteboard.image = iv.image 
    end
  end

  def cut(sender)
    iv = self.imageContainsPoint(@touch_point)
    if iv
      puts "cut"
      UIPasteboard.generalPasteboard.image = iv.image if iv
      iv.removeFromSuperview
    end
  end

  def paste(sender)
    pb = UIPasteboard.generalPasteboard
    if pb.image
      puts "paste"
      bug = UIImageView.alloc.initWithImage(pb.image)
      bug.center = @touch_point
      self.view.addSubview(bug)
      pb.image = bug.image
    end
  end

end
