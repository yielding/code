class DrawImageTest < UIView 
  def initWithFrame(frame)
    super 
    self
  end

  def initWithImage(image, frame)
    @image = image
    initWithFrame(frame)
  end

  def drawRect(rect)
    @image.drawAtPoint(rect.origin)
  end
end 

class SampleForUIImage < UIViewController
  def viewDidLoad
    super
    image = UIImage.imageNamed("doc.jpg")
    test = DrawImageTest.alloc.initWithImage(image, view.bounds)
    test.frame = view.bounds
    test.autoresizingMask =
      UIViewAutoresizingFlexibleWidth or 
      UIViewAutoresizingFlexibleHeight
    self.view.addSubview(test)
  end
end
