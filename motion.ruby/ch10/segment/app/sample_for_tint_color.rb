class SampleForTintColor < UIViewController
  def viewDidLoad
    super
    view.backgroundColor = UIColor.whiteColor
    items = ["plain", "bordered", "bar"]
    segment = UISegmentedControl.alloc.initWithItems(items)
    segment.segmentedControlStyle = UISegmentedControlStylePlain
    segment.tintColor = UIColor.redColor
    segment.selectedSegmentIndex = 0
    segment.frame = [[10, 50], [300, 30]]
    segment.addTarget(self, action:"segmentDidChange:",
                      forControlEvents:UIControlEventValueChanged)
    view.addSubview(segment)
  end

  def segmentDidChange(s)
    segments = [UISegmentedControlStylePlain, UISegmentedControlStyleBordered,
                UISegmentedControlStyleBar]

    s.segmentedControlStyle = segments[s.selectedSegmentIndex]
  end
end
