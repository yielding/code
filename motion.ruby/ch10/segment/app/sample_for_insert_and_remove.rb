class SampleForInsertAndRemove < UIViewController
  def viewDidLoad
    super

    view.backgroundColor = UIColor.whiteColor
    @segment = UISegmentedControl.new
    @segment.segmentedControlStyle = UISegmentedControlStyleBordered
    @segment.frame = [[10, 50], [300, 30]]
    view.addSubview(@segment)

    @segment.insertSegmentWithTitle("3", atIndex:0, animated:false)
    @segment.insertSegmentWithTitle("2", atIndex:0, animated:false)
    @segment.insertSegmentWithTitle("1", atIndex:0, animated:false)

    ib = UIBarButtonItem.alloc.initWithTitle("Insert",
                                       style:UIBarButtonItemStyleBordered,
                                       target:self,
                                       action:"insertDidPush")
    rb = UIBarButtonItem.alloc.initWithTitle("Remove",
                                       style:UIBarButtonItemStyleBordered,
                                       target:self,
                                       action:"removeDidPush")
    rab = UIBarButtonItem.alloc.initWithTitle("RemoveAll",
                                       style:UIBarButtonItemStyleBordered,
                                       target:self,
                                       action:"removeAllDidPush")
    items = [ib, rb, rab]
    self.setToolbarItems(items, animated:true)
  end

  def insertDidPush
    n = @segment.numberOfSegments + 1
    @segment.insertSegmentWithTitle(n.to_s, atIndex:n, animated:true)
  end

  def removeDidPush
    @segment.removeSegmentAtIndex(@segment.numberOfSegments - 1, animated:true)
  end

  def removeAllDidPush
    @segment.removeAllSegments
  end
end
