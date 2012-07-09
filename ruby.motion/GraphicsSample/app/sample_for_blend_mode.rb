class BlendModeTest < UIView 
  def init
    if super
      @back_iamge = UIImage.imageNamed("back.png")
      @fore_image = UIImage.imageNamed("dog.png")
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

  end
end
