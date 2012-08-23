class SampleForContentOffset < UIViewController
  def viewDidLoad
    super
    view.backgroundColor = UIColor.whiteColor

    items = ["LEVEL1", "LEVEL2", "LEVEL3"]
    segment = UISegmentedControl.alloc.initWithItems(items)
    segment.segmentedControlStyle = UISegmentedControlStyleBar
    segment.selectedSegmentIndex  = 0
    segment.frame = [[10, 50], [300, 40]]

    segment.setContentOffset([0, -7], forSegmentAtIndex:0)
    segment.setContentOffset([0,  7], forSegmentAtIndex:2)

    view.addSubview(segment)
  end
end
