class SampleForMomentary < UIViewController
  def viewDidLoad
    super
    self.view.backgroundColor = UIColor.blackColor

    items   = ["Black", "White"]
    segment = UISegmentedControl.alloc.initWithItems(items)
    segment.momentary = true
    segment.frame     = [[0, 0], [130, 30]]
    segment.addTarget(self, action:"segmentDidChange:", 
                            forControlEvents:UIControlEventValueChanged)
    bb = UIBarButtonItem.alloc.initWithCustomView(segment)
    self.navigationItem.rightBarButtonItem = bb
  end

  def segmentDidChange(s)
    if s.class == UISegmentedControl
      if s.selectedSegmentIndex == 0
        view.backgroundColor = UIColor.blackColor
      else
        view.backgroundColor = UIColor.whiteColor
      end
    end
  end
end
