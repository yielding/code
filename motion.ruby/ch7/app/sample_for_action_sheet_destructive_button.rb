class SampleForActionSheetWithDestructiveButton < UIViewController
  def viewWillAppear animated
    super animated
    self.navigationController.setToolbarHidden(false, animated:true)
  end

  def viewDidLoad
    super
    self.title = "액션시트"
    button = UIBarButtonItem.alloc.initWithBarButtonSystemItem(
                UIBarButtonSystemItemTrash,
                target: self,
                action: :"buttonDidPush")
    # Test
    self.setToolbarItems([button], animated:true)
  end

  def buttonDidPush
    sheet = UIActionSheet.alloc.init 
    sheet.delegate = self
    sheet.addButtonWithTitle "삭제한다"
    sheet.addButtonWithTitle "취소"
    sheet.destructiveButtonIndex = 0
    sheet.cancelButtonIndex = 1
    sheet.showFromToolbar(self.navigationController.toolbar)
  end

  def actionSheet(as, clickedButtonAtIndex:index)
    if (index == as.destructiveButtonIndex)
      puts "pushed Delete button"
    else
      puts "pushed Cancel button"
    end

  end
end
