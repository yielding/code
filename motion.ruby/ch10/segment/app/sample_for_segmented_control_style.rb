class SampleForSegmentedControlStyle < UIViewController
  def viewDidLoad
    super

    view.backgroundColor = UIColor.whiteColor
    items = ["Plain", "Bordered", "Bar"]
    segment = UISegmentedControl.alloc.initWithItems(items)
    segment.segmentedControlStyle = UISegmentedControlStylePlain
    segment.selectedSegmentIndex  = 0
    segment.frame = [[10, 50], [300, 30]]
    segment.addTarget(self, action:"segmentDidChange:",
                      forControlEvents:UIControlEventValueChanged)
    view.addSubview(segment)
  end

  def segmentDidChange(s)
    if s.class.eql? UISegmentedControl
      styles = [UISegmentedControlStylePlain, UISegmentedControlStyleBordered,
                UISegmentedControlStyleBar]
      s.segmentedControlStyle = styles[s.selectedSegmentIndex]
    end
  end
end
