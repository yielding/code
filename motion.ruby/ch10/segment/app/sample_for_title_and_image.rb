class SampleForTitleAndImage < UIViewController
  def viewDidLoad
    super

    @titles = ["Elephant", "Lion", "Dog"]
    @images = @titles.map { |e| UIImage.imageNamed("#{e}.png") }

    segment = UISegmentedControl.alloc.initWithItems(@titles)
    segment.segmentedControlStyle = UISegmentedControlStyleBordered
    segment.frame = [[10, 50], [300, 30]]
    segment.addTarget(self, action:"segmentDidChange:",
                            forControlEvents:UIControlEventValueChanged)
    view.addSubview(segment)
  end

  def segmentDidChange(s)
    return unless s.class == UISegmentedControl

    for i in (0...s.numberOfSegments)
      if s.selectedSegmentIndex == i
        s.setImage(@images[i], forSegmentAtIndex:i)
      else
        s.setTitle(@titles[i], forSegmentAtIndex:i)
      end
    end
  end
end
