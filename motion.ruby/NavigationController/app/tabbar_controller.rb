class TabBar < UITabBarController
  def viewDidLoad
    super
    self.title = "UITabBarController"
    s1  = Scene.alloc.initWithSystemItem(UITabBarSystemItemFeatured, badge:nil)
    s2  = Scene.alloc.initWithSystemItem(UITabBarSystemItemMostViewed, badge:"1")
    s3  = Scene.alloc.initWithSystemItem(UITabBarSystemItemSearch, badge:nil)
    #s4  = Scene.alloc.initWithSystemItem(UITabBarSystemItemBookmarks, badge:nil)
    s5  = Scene.alloc.initWithSystemItem(UITabBarSystemItemMostRecent, badge:"2")
    s6  = Scene.alloc.initWithSystemItem(UITabBarSystemItemTopRated, badge:nil)
    s7  = Scene.alloc.initWithSystemItem(UITabBarSystemItemHistory, badge:nil)
    s8  = Scene.alloc.initWithSystemItem(UITabBarSystemItemDownloads, badge:nil)
    s9  = Scene.alloc.initWithSystemItem(UITabBarSystemItemContacts, badge:nil)
    s12 = Scene.alloc.initWithFileName("smile.png", title:"스마일")
    
    #scenes = [s1, s2, s3, s4, s5, s6, s7, s8, s9, s12]
    scenes = [s1, s2, s3, s5, s6, s7, s8, s9, s12]

    self.setViewControllers(scenes, animated:false)
    #self.customizableViewControllers = [s4, s5, s6, s7, s8, s9, s12]
    self.customizableViewControllers = [s5, s6, s7, s8, s9, s12]
  end

  def viewWillAppear(animated)
    super
    self.navigationController.setNavigationBarHidden(true, animated:false)
    self.navigationController.setToolbarHidden(true, animated:false)
  end
      
end

class Scene < UIViewController
  def initWithSystemItem(item, badge:bg)
    if self.init
      self.tabBarItem = UITabBarItem.alloc.initWithTabBarSystemItem(item, tag:0)
      self.tabBarItem.badgeValue = bg
    end
    self
  end

  def initWithFileName(fname, title:t)
    if self.init
      icon = UIImage.imageNamed(fname)
      self.tabBarItem = UITabBarItem.alloc.initWithTitle(t, image:icon, tag:0)
    end
    self
  end

  def viewDidLoad
    super
    label = UILabel.alloc.init
    label.frame = self.view.bounds
    label.autoresizingMask = UIViewAutoresizingFlexibleWidth or UIViewAutoresizingFlexibleHeight
    label.backgroundColor  = UIColor.blackColor
    self.view.addSubview(label)
  end

  def touchesEnded(touches, withEvent:event)
    self.navigationController.setNavigationBarHidden(false, animated:true)
  end
end
