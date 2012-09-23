class SampleForIndicatorStyle < UIViewController
  def viewDidLoad
    super
    @sv = UIScrollView.new
    @sv.frame = self.view.bounds
    @sv.autoresizingMask = UIViewAutoresizingFlexibleWidth |
                           UIViewAutoresizingFlexibleHeight
    view = UIView.new
    view.frame = [[0, 0], [800, 600]]
    view.backgroundColor = UIColor.grayColor

    @sv.contentSize = view.bounds.size
    @sv.addSubview(view)
    self.view.addSubview(@sv)

    btn = UIBarButtonItem.alloc.initWithTitle("Style전환",
                                              style:UIBarButtonItemStyleBordered,
                                              target:self,
                                              action:"changeButtonDidPush")
    self.setToolbarItems([btn])
  end

  def changeButtonDidPush
    @sv.indicatorStyle += 1
    if @sv.indicatorStyle > UIScrollViewIndicatorStyleWhite
       @sv.indicatorStyle = UIScrollViewIndicatorStyleDefault 
    end
  end
end
