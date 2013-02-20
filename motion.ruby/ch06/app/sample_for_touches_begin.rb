class SampleForTouchesBegin < UIViewController
  def viewDidLoad
    super
    view.backgroundColor = UIColor.whiteColor
  end

  def touchesBegan(touches, withEvent:event)
    alert = UIAlertView.alloc.initWithTitle(nil, 
              message:"I'm a view Controller",
              delegate:nil,
              cancelButtonTitle:nil,
              otherButtonTitles:"ok",
              nil)
    alert.show
  end
end
