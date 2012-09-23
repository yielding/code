class MyViewController < UIViewController
  attr_accessor :number

  def self.myViewControllerWithNumber(no)
    my_vc = MyViewController.alloc.init
    my_vc.number = no
    my_vc
  end

  def viewDidLoad
    super

    label = UILabel.alloc.init
    label.frame = self.view.bounds
    label.autoresizingMask = UIViewAutoresizingFlexibleWidth or UIViewAutoresizingFlexibleHeight
    label.backgroundColor  = @number.even? ? UIColor.whiteColor : UIColor.blackColor
    label.textColor        = @number.even? ? UIColor.blackColor : UIColor.whiteColor
    label.textAlignment    = UITextAlignmentCenter
    label.font = UIFont.boldSystemFontOfSize(128)
    label.text = @number.to_s
    self.view.addSubview(label)
  end
end

class SampleForPaging < UIViewController
  NUMBER_OF_PAGES = 3
  VIEW_HEIGHT     = 360

  def viewDidLoad
    super

    sv = UIScrollView.new
    sv.frame = self.view.bounds
    sv.autoresizingMask = UIViewAutoresizingFlexibleWidth or 
                          UIViewAutoresizingFlexibleHeight
    sv.contentSize   = [320 * NUMBER_OF_PAGES, VIEW_HEIGHT]
    sv.pagingEnabled = true
    sv.showsHorizontalScrollIndicator = false
    sv.showsVerticalScrollIndicator   = false
    sv.scrollsToTop = false
    for i in 0...NUMBER_OF_PAGES 
      myvc = MyViewController.myViewControllerWithNumber(i)
      myvc.view.frame = [[320*i, 0], [320, VIEW_HEIGHT]]
      sv.addSubview(myvc.view)
    end

    self.view.addSubview(sv)
  end

end
