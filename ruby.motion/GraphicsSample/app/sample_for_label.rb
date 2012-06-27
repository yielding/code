class SampleForLabel < UIViewController
  def viewDidLoad
    super
    label = UILabel.alloc.init
    label.frame = self.view.bounds
    label.autoresizingMask = UIViewAutoresizingFlexibleWidth or
                             UIViewAutoresizingFlexibleHeight
    label.textAlignment = UITextAlignmentCenter
    label.text = "또 다시 라벨이 등장합니다. 여기서 잘 공부하는 거지"
    self.view.addSubview(label)
  end
end
