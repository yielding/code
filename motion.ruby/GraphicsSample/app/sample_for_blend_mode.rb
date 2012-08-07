class BlendModeTest < UIView 
  attr_reader :blend_mode
  def init
    if super
      @back_image = UIImage.imageNamed("back.png")
      @fore_image = UIImage.imageNamed("dog.jpg")
      @blend_mode = KCGBlendModeNormal
      new_frame = self.frame
      new_frame.size = @fore_image.size
      self.frame = new_frame
    end

    self
  end

  def drawRect(rect)
    @back_image.drawInRect(rect)
    @fore_image.drawInRect(rect, blendMode:@blend_mode, alpha:1.0)
  end

  def change_mode
    @blend_mode += 1
    @blend_mode = KCGBlendModeNormal if KCGBlendModeLuminosity < @blend_mode 
  end

end

class SampleForBlendMode < UIViewController
  def viewDidLoad
    super
    @t = BlendModeTest.alloc.init 
    @t.center = self.view.center
    @t.autoresizingMask = UIViewAutoresizingFlexibleTopMargin or
                          UIViewAutoresizingFlexibleBottomMargin
    view.addSubview(@t)
    @label = UILabel.alloc.init 
    @label.frame = [[0, view.bounds.size.height - 100], [view.bounds.size.width, 20]]
    @label.autoresizingMask = UIViewAutoresizingFlexibleTopMargin or
                              UIViewAutoresizingFlexibleBottomMargin
    @label.textAlignment    = UITextAlignmentCenter
    view.addSubview(@label)
    change_label

    back_image = UIImage.imageNamed("back.png")
    back_iview = UIImageView.alloc.initWithImage(back_image)
    back_iview.frame = [[0, 0], [90, 83]]
    back_iview.autoresizingMask \
      = UIViewAutoresizingFlexibleLeftMargin or
        UIViewAutoresizingFlexibleRightMargin or
        UIViewAutoresizingFlexibleTopMargin or
        UIViewAutoresizingFlexibleBottomMargin

    view.addSubview(back_iview)
    fore_image = UIImage.imageNamed("dog.jpg")
    fore_iview = UIImageView.alloc.initWithImage(fore_image)
    fore_iview.frame = [[320 - 90, 0], [90, 83]]
    #fore_iview.autoresizingMask = 
    view.addSubview(fore_iview)
  end

  def change_label
    txt = case @t.blend_mode
          when KCGBlendModeMultiply;        "kCGBlendModeMultiply"
          when KCGBlendModeScreen;          "kCGBlendModeScreen"
          when KCGBlendModeOverlay;         "kCGBlendModeOverlay"
          when KCGBlendModeDarken;          "kCGBlendModeDarken"
          when KCGBlendModeLighten;         "kCGBlendModeLighten"
          when KCGBlendModeColorDodge;      "kCGBlendModeColorDodge"
          when KCGBlendModeColorBurn;       "kCGBlendModeColorBurn"
          when KCGBlendModeSoftLight;       "kCGBlendModeSoftLight"
          when KCGBlendModeHardLight;       "kCGBlendModeHardLight"
          when KCGBlendModeDifference;      "kCGBlendModeDifference"
          when KCGBlendModeExclusion;       "kCGBlendModeExclusion"
          when KCGBlendModeHue;             "kCGBlendModeHue"
          when KCGBlendModeSaturation;      "kCGBlendModeSaturation"
          when KCGBlendModeColor;           "kCGBlendModeColor"
          when KCGBlendModeLuminosity;      "kCGBlendModeLuminosity"
          when KCGBlendModeClear;           "kCGBlendModeClear"
          when KCGBlendModeCopy;            "kCGBlendModeCopy"
          when KCGBlendModeSourceIn;        "kCGBlendModeSourceIn"
          when KCGBlendModeSourceOut;       "kCGBlendModeSourceOut"
          when KCGBlendModeSourceAtop;      "kCGBlendModeSourceAtop"
          when KCGBlendModeDestinationOver; "kCGBlendModeDestinationOver"
          when KCGBlendModeDestinationIn;   "kCGBlendModeDestinationIn"
          when KCGBlendModeDestinationOut;  "kCGBlendModeDestinationOut"
          when KCGBlendModeDestinationAtop; "kCGBlendModeDestinationAtop"
          when KCGBlendModeXOR;             "kCGBlendModeXOR"
          when KCGBlendModePlusDarker;      "kCGBlendModePlusDarker"
          when KCGBlendModePlusLighter;     "kCGBlendModePlusLighter"
          else
            "KCGBlendModeNormal"
          end
    txt 
  end

  def touchesEnded(touches, withEvent:event)
    @t.change_mode
    change_label
    @t.setNeedsDisplay
  end
end
