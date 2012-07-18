class SampleForActionSheetObserving < UIViewController
  def viewWillAppear animated
    super animated
    self.navigationController.setToolbarHidden(false, animated:true)
    self.title = "액션시트"

    btn = UIBarButtonItem.alloc.initWithBarButtonSystemItem(
            UIBarButtonSystemItemAction,
            target:self,
            action:"buttonDidPush")
    self.setToolbarItems([btn], animated:true)
  end

  def buttonDidPush
    sheet = UIActionSheet.alloc.init 
    sheet.delegate = self
    sheet.addButtonWithTitle "답장"
    sheet.addButtonWithTitle "전송"
    sheet.addButtonWithTitle "취소"
    sheet.cancelButtonIndex = 2
    sheet.showFromToolbar self.navigationController.toolbar 
  end

  def actionSheet(as, clickedButtonAtIndex:index)
    msg = (index == as.cancelButtonIndex) ? "pushed Cancel Button" : "pushed other Button."
    puts msg
  end

  def willPresentActionSheet(as)
    puts "willPresentActionSheet"
  end

  def didPresentActionSheet(as)
    puts "didPresentActionSheet"
  end

  def actionSheetCancel(as)
    puts "actionSheetCancel"
  end

  def actionSheet(as, willDismissWithButtonIndex:index)
    puts "willDismissWithButtonIndex"
  end

  def actionSheet(as, didDismissWithButtonIndex:index)
    puts "didDismissWithButtonIndex"
  end

  def shouldAutorotateToInterfaceOrientation(ori)
    true 
  end
end
