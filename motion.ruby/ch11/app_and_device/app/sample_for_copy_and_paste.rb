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
    if action == :"copy:"
      return true if self.imageContainsPoint(@touch_point)
    elsif action == :"cut:"
      return true if self.imageContainsPoint(@touch_point)
    elsif action == :"paste:"
      return true unless UIPasteboard.generalPasteboard.image.nil?
    end

    false
  end

  def imageContainsPoint(point)
    view.subviews.each { |view|  
      if CGRectContainsPoint(view.frame, point)
        if view.class == UIImageView
          puts "4"
          return view
        end
      end
    }

    nil
  end

  def copy(sender)
    puts "in copy"
    iv = self.imageContainsPoint(@touch_point)
    if iv
      puts "before copy"
      UIPasteboard.generalPasteboard.image = iv.image 
      puts "after copy"
    end
  end

  def cut(sender)
    puts "in cut"
    iv = self.imageContainsPoint(@touch_point)
    if iv
      UIPasteboard.generalPasteboard.image = iv.image if iv
      iv.removeFromSuperview
    end
  end

  def paste(sender)
    puts "in paste"
    pb = UIPasteboard.generalPasteboard
    if pb.image
      puts "has image"
      bug = UIImageView.alloc.initWithImage(pb.image)
      bug.center = @touch_point
      self.view.addSubview(bug)
      pb.image = bug.image
      puts "bingo"
    end
  end

end
