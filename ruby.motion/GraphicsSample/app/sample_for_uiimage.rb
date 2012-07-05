<<<<<<< HEAD
#class DrawImageTest < UIImageView 
class DrawImageTest < UIImageView 
  def initWithImage(image)
    @image = image if super.init
=======
class DrawImageTest < UIImageView
  def initWithImage(image)
    @image = image if super 
>>>>>>> 59529a34ade08acc4aea1cb76a7a2aeb6b0d9949
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
<<<<<<< HEAD
    test  = DrawImageTest.alloc.initWithImage(image)
    test.frame = self.view.bounds
=======
    test = DrawImageTest.alloc.initWithImage(image)
    test.frame = view.bounds
>>>>>>> 59529a34ade08acc4aea1cb76a7a2aeb6b0d9949
    test.autoresizingMask =
      UIViewAutoresizingFlexibleWidth or 
      UIViewAutoresizingFlexibleHeight
    self.view.addSubview(test)
  end
end
