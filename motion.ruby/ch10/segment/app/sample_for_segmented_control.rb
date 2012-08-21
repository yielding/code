class SampleForSegmentedControl < UIViewController
  def viewDidLoad
    super

    self.view.backgroundColor = UIColor.blackColor
    items   = ["black", "white"]
    segment = UISegmentedControl.alloc.initWithItems(items)
    segment.selectedSegmentIndex = 0
    segment.frame = [[0, 0], [130, 30]]
    segment.addTarget(self, action:"segmentDidChange:", 
                            forControlEvents:UIControlEventValueChanged)
    bar_button = UIBarButtonItem.alloc.initWithCustomView(segment)
    self.navigationItem.rightBarButtonItem = bar_button
  end

  def segmentDidChange(segment)
    if segment.class == UISegmentedControl
      if 0 == segment.selectedSegmentIndex
        view.backgroundColor = UIColor.blackColor 
      else
        view.backgroundColor = UIColor.whiteColor
      end
    end
  end
end
