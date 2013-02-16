class SampleForFrame < UIViewController
  def viewDidLoad
    super
    view.backgroundColor = UIColor.blackColor
    l1 = UILabel.alloc.initWithFrame(CGRectZero)
    l1.text = "거의 오른쪽 위"

    new_frame = [[220, 20], [100, 50]]
    l1.frame = new_frame

    l2 = UILabel.alloc.initWithFrame(l1.frame)
    l2.textAlignment = UITextAlignmentCenter
    l2.text = "세계의 중심"

    new_point = view.center
    new_point.y -= 20
    l2.center = new_point

    view.addSubview(l1)
    view.addSubview(l2)
  end

  def touchesEnded(touches, withEvent:e)
    self.navigationController.setNavigationBarHidden(false, animated:true)
  end
end
