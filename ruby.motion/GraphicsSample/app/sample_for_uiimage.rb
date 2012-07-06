class DrawImageTest < UIImageView
  def initWithImage(image)
    @image = image if super 
    self
  end

  def drawRect(rect)
    @image.drawAtPoint(rect.origin) unless @image.nil?
  end
end 

class SampleForUIImage < UIViewController
  def viewDidLoad
    super
    image = UIImage.imageNamed("dog.jpg")
    test = DrawImageTest.alloc.initWithImage(image)
    test.frame = view.bounds
    test.autoresizingMask = UIViewAutoresizingFlexibleWidth or 
                            UIViewAutoresizingFlexibleHeight
    self.view.addSubview(test)
  end
end
