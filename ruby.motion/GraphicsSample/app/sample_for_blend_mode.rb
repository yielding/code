class BlendModeTest < UIView 
  def init
    if super
      @back_iamge = UIImage.imageNamed("back.png")
      @fore_image = UIImage.imageNamed("dog.png")
      @blend_mode = KCGBlendModeNormal
      new_frame = self.frame
      new_frame.size = @fore_image.size
      self.frame = new_frame
    end

    self
  end

  def drawRect(rect)
    @back_iamge.drawInRect(rect)
    @fore_image.drawInRect(rect, blendMode:@blend_mode, alpha:1.0)
  end

  def changeMode
    @blend_mode += 1
    @blend_mode = kCGBlendModeNormal if KCGBlendModeLuminosity < @blend_mode 
  end

end

class SampleForBlendMode < UIViewController
  def viewDidLoad
    super
    t = BlendModeTest.alloc.init 
    t.center = self.view.center
    t.autoresizingMask = UIViewAutoresizingFlexibleTopMargin or
                         UIViewAutoresizingFlexibleBottomMargin
    view.addSubview(t)
    label = UILabel.alloc.init 
    label.frame = [[0, view.bounds.size.height - 100], [view.bounds.size.width, 20]]
    label.autoresizingMask = UIViewAutoresizingFlexibleTopMargin or
                             UIViewAutoresizingFlexibleBottomMargin
    label.textAlignment = UITextAlignmentCenter
    view.addSubview(label)
    change_label

    back_image = UIImage.imageNamed("back.png")
    back_iview = UIImageView.alloc.initWithImage(back_image)
    back_iview.frame = [[0, 0], [90, 83]]

    # back_iview.frame = [[320 - 90, 0], [90, 83]]

  end

  def change_label
  end
end
