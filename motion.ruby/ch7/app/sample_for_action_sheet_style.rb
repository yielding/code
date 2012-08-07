class SampleForActionSheetStyle < UIViewController 
  def viewWillApear(animated)
    super(animated)
    self.navigationController.setToolbarHidden(true, animated:true)
  end

  def viewDidLoad
    super
    @action_sheet_style = UIActionSheetStyleAutomatic 
    self.title = "Action Sheet"
    btn = UIBarButtonItem.alloc.initWithBarButtonSystemItem(UIBarButtonSystemItemAction,
            target:self, 
            action:"buttonDidPush")
    self.navigationItem.rightBarButtonItem = btn 

    img = UIImage.imageNamed("town.jpg")
    img_view = UIImageView.alloc.initWithImage(img)
    img_view.frame = self.view.bounds
    img_view.contentMode = UIViewContentModeScaleAspectFill
    img_view.autoresizingMask =
      UIViewAutoresizingFlexibleWidth or UIViewAutoresizingFlexibleHeight 
    self.view.addSubview(img_view)
  end

  def buttonDidPush
    sheet = UIActionSheet.alloc.init 
    sheet.delegate = self 
    sheet.actionSheetStyle = @action_sheet_style 
    sheet.title = "UIActionSheetStyle을 선택하세요"

    sheet.addButtonWithTitle("StyleDefault")
    sheet.addButtonWithTitle("StyleBalckTranslucent")
    sheet.addButtonWithTitle("StyleBalckOpaque")
    sheet.addButtonWithTitle("Cancel")
    sheet.cancelButtonIndex = 3
    sheet.showInView(self.view)
  end

  def actionSheet(sheet, clickedButtonAtIndex: index)
    case index 
    when 0; @action_sheet_style = UIActionSheetStyleDefault;
    when 1; @action_sheet_style = UIActionSheetStyleBlackTranslucent;
    when 2; @action_sheet_style = UIActionSheetStyleBlackOpaque;
    else 
            @action_sheet_style = UIActionSheetStyleAutomatic;
    end
  end
end
