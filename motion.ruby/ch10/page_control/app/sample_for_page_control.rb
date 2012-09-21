class SampleForPageControl < UIViewController
  def viewDidLoad
    super
    @pc = UIPageControl.new
    @pc.frame = [[0, view.bounds.size.height - 30], [320, 30]]
    @pc.backgroundColor = UIColor.blackColor
    @pc.addTarget(self, action:"pageControlDidChange:", 
                  forControlEvents:UIControlEventValueChanged)

    @pc.autoresizingMask = UIViewAutoresizingFlexibleTopMargin |
                           UIViewAutoresizingFlexibleBottomMargin
    @pc.numberOfPages = 5
    @pc.currentPage = 2
    self.view.addSubview(@pc)

    add_btn = UIBarButtonItem.alloc.initWithTitle("페이지추가",
                  style:UIBarButtonItemStyleBordered,
                 target:self,
                 action:'addDidPush')

    del_btn = UIBarButtonItem.alloc.initWithTitle("페이지삭제",
                  style:UIBarButtonItemStyleBordered,
                 target:self,
                 action:'delDidPush')

    self.setToolbarItems([add_btn, del_btn], animated:false)
  end

  def pageControlDidChange(sender)
  end

  def addDidPush
    @pc.numberOfPages += 1 if @pc.numberOfPages < 10
  end

  def delDidPush
    if @pc.numberOfPages > 1
      @pc.numberOfPages -= 1 
      self.pageControlDidChange(@pc)
    end
  end
end
