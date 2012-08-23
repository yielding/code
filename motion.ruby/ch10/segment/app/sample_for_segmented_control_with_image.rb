class SampleForSegmentedControlWithImage < UIViewController
  def viewDidLoad
    super
    view.backgroundColor = UIColor.whiteColor

    i1 = UIImage.imageNamed("Elephant.png")
    i2 = UIImage.imageNamed("Lion.png")
    i3 = UIImage.imageNamed("Dog.png")

    segment = UISegmentedControl.alloc.initWithItems([i1, i2, i3])
    segment.segmentedControlStyle = UISegmentedControlStyleBar
    segment.selectedSegmentIndex  = 0
    segment.frame = [[60, 50], [200, 40]]

    view.addSubview segment 
  end
end
