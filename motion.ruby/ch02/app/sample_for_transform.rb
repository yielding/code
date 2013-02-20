class SampleForTransform < UIViewController
  def viewDidLoad
    super

    @rotate = 0.0
    @scale  = 1.0
    @need_flip = false

    view.backgroundColor = UIColor.blackColor

    path  = "#{NSBundle.mainBundle.resourcePath}/dog.jpg"
    image = UIImage.alloc.initWithContentsOfFile(path)

    @image_view = UIImageView.alloc.initWithImage(image)
    np   = view.center
    np.y = view.center.y - 60
    @image_view.center = np

    view.addSubview(@image_view)

    rotate_btn = UIButton.buttonWithType(UIButtonTypeRoundedRect)
    rotate_btn.frame = CGRectMake(0, 0, 50, 40)

    np = view.center
    np.x -= 75
    np.y  = view.frame.size.height - 70
    rotate_btn.center = np
    rotate_btn.setTitle("회전", forState:UIControlStateNormal)
    rotate_btn.addTarget(self, 
                         action:'rotateDidPush',
                         forControlEvents:UIControlEventTouchUpInside)
    view.addSubview(rotate_btn)

    big_btn = UIButton.buttonWithType(UIButtonTypeRoundedRect)
    big_btn.frame = rotate_btn.frame
    np.x += 50
    big_btn.center = np
    big_btn.setTitle("확대", forState:UIControlStateNormal)
    big_btn.addTarget(self, 
                      action:'bigDidPush',
                      forControlEvents:UIControlEventTouchUpInside)
    view.addSubview(big_btn)

    small_btn = UIButton.buttonWithType(UIButtonTypeRoundedRect)
    small_btn.frame = rotate_btn.frame
    np.x += 50
    small_btn.center = np
    small_btn.setTitle("축소", forState:UIControlStateNormal)
    small_btn.addTarget(self, 
                      action:'smallDidPush',
                      forControlEvents:UIControlEventTouchUpInside)
    view.addSubview(small_btn)

    invert_btn = UIButton.buttonWithType(UIButtonTypeRoundedRect)
    invert_btn.frame = rotate_btn.frame
    np.x += 50
    invert_btn.center = np
    invert_btn.setTitle("반전", forState:UIControlStateNormal)
    invert_btn.addTarget(self, 
                      action:'invertDidPush',
                      forControlEvents:UIControlEventTouchUpInside)
    view.addSubview(invert_btn)
  end

  def rotateDidPush
    @rotate += 90.0
    @rotate  = 0.0 if (@rotate > 359.0)

    transformWithAnimation
  end

  def bigDidPush
    @scale += 0.1
    transformWithAnimation
  end

  def smallDidPush
    @scale -= 0.1
    transformWithAnimation
  end

  def invertDidPush
    @need_flip = !@need_flip
    transformWithAnimation
  end

  def transformWithAnimation
    UIView.beginAnimations(nil, context:nil)
    tr_rotate = CGAffineTransformMakeRotation(@rotate * (Math::PI / 180.0))
    tr_scale  = CGAffineTransformMakeScale(@scale, @scale)
    tr_all    = CGAffineTransformConcat(tr_rotate, tr_scale)
    if @need_flip
      tr_all = CGAffineTransformScale(tr_all, -1.0, 1.0)
    end

    @image_view.transform = tr_all

    UIView.commitAnimations
  end

  def touchesEnded(touches, withEvent:e)
    navigationController.setNavigationBarHidden(false, animated:true)
  end
end

