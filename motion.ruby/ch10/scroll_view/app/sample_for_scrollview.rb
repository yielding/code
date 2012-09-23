class SampleForScrollView < UIViewController
  def viewDidLoad
    super

    sv = UIScrollView.new
    sv.frame = self.view.bounds
    sv.autoresizingMask = UIViewAutoresizingFlexibleWidth or
                          UIViewAutoresizingFlexibleHeight
    image = UIImage.imageNamed("town.jpg")
    image_view = UIImageView.alloc.initWithImage(image)
    sv.addSubview(image_view)
    sv.contentSize = image_view.bounds.size
    
    self.view.addSubview(sv)
    sv.delegate = self
    sv.minimumZoomScale = 0.1
    sv.maximumZoomScale = 3.0
  end

  def viewForZoomingInScrollView sv
    sv.subviews.each { |subview|
      return subview if subview.class.eql? UIImageView
    }
    nil
  end
end
