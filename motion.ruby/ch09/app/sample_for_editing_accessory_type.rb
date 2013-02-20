class SampleForEditingAccessoryType < SampleForCellWithDetail
  def viewDidLoad
    super

    @details = ["13", "1", "0", "255"]
    @cell_style = UITableViewCellStyleValue1
    self.navigationItem.rightBarButtonItem = self.editButtonItem
    self.tableView.allowsSelection = false
    self.tableView.allowsSelectionDuringEditing = true
  end

  def tableView(tv, cellForRowAtIndexPath:ip)
    cell = super
    cell.editingAccessoryType = UITableViewCellAccessoryDetailDisclosureButton
    cell
  end

  def tableView(tv, editingStyleForRowAtIndexPath:ip)
    UITableViewCellEditingStyleNone
  end

  def tableView(tv, shouldIndentWhileEditingRowAtIndexPath:ip)
    false
  end

  def tableView(tv, accessoryButtonTappedForRowWithIndexPath:ip)
  end
end
