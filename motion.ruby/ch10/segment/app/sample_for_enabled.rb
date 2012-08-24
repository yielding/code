class SampleForEnabled < UIViewController
  def viewDidLoad
    super
    view.backgroundColor = UIColor.whiteColor
    items = [ "Enabled", "Disabled", "Enabled"]

    segment = UISegmentedControl.alloc.initWithItems(items)
    segment.segmentedControlStyle = UISegmentedControlStyleBordered
    segment.selectedSegmentIndex = 0
    segment.frame = [[10, 50], [300, 40]]
    segment.setEnabled(false, forSegmentAtIndex:1)

    view.addSubview(segment)
  end
end
