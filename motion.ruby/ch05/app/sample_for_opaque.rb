class OpaqueTest < UIView 
  def drawRect(rect)
    system_font = UIFont.systemFontOfSize(UIFont.systemFontSize)
    "OPAQUE".drawAtPoint([0, 0], withFont:system_font)
  end
end

class SampleForOpaque < UIViewController
  def viewDidLoad
    super
    self.view.backgroundColor = UIColor.grayColor
    t11 = OpaqueTest.alloc.init
    t11.opaque = true
    t11.frame = [[50, 50], [100, 100]]

    t12 = OpaqueTest.alloc.init 
    t12.opaque = false
    t12.frame = [[150, 50], [100, 100]]

    t21 = OpaqueTest.alloc.init 
    t21.opaque = true
    t21.alpha  = 0.0
    t21.frame = [[50, 150], [100, 100]]

    t22 = OpaqueTest.alloc.init 
    t22.opaque = true
    t22.backgroundColor = UIColor.whiteColor
    t22.frame = [[150, 150], [100, 100]]

    self.view.addSubview(t11)
    self.view.addSubview(t12)
    self.view.addSubview(t21)
    self.view.addSubview(t22)
  end
end
