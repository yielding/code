class SampleForEditingAccessoryType < UITableViewController
  def viewDidLoad
    super

    @details = ["13", "1", "0", "255"]
    @cell_style = UITableViewCellStyleValue1
    self.navigationItem.rightBarButtonItem = self.editButtonItem
    self.tableView.allowsSelection = true
    self.tableView.allowsSelectionDuringEditing = true
  end

  def tableView(tv, cellForRowAtIndexPath:ip)
    cell = tableView(tv, cellForRowAtIndexPath:ip)
    cell.editingAccessoryType = UITableViewCellAccessoryDetailDisclosureButton
    cell
  end

  def tableView(tv, editingStyleForRowAtIndexPath:ip)
    UITableViewCellEditingstyleNone
  end

  def tableView(tv, shouldIndentWhileEditingRowAtIndexPath:ip)
    false
  end

  def tableView(tv, accessoryButtonTappedForRowWithIndexPath:ip)
  end
end
