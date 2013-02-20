class SampleForUIImageViewAnimation < UIViewController 
  def viewWillAppear(animated)
    super(animated)
    @image_view.startAnimating unless @image_view.nil?
  end

  def viewWillDisappear(animated)
    super(animated)
    @image_view.stopAnimating unless @image_view.nil?
  end

  def viewDidLoad
    super 
    view.backgroundColor = UIColor.whiteColor
    @image_view = UIImageView.alloc.init 
    @image_view.frame = [[0, 0], [64, 64]]
    @image_view.animationImages = [ 
      UIImage.imageNamed("chara1.png"), 
      UIImage.imageNamed("chara2.png")
    ]
    @image_view.animationDuration = 0.2
    @image_view.center = self.view.center
    @image_view.autoresizingMask  =
      UIViewAutoresizingFlexibleLeftMargin or 
      UIViewAutoresizingFlexibleRightMargin or 
      UIViewAutoresizingFlexibleTopMargin or 
      UIViewAutoresizingFlexibleBottomMargin
    view.addSubview(@image_view)
  end
end
