class SampleForEditingButton < SampleForMovableRow
  def viewDidLoad
    super
    @data_source = ["ITEM 1", "ITEM 2", "ITEM 3"]
    self.navigationItem.rightBarButtonItem = self.editButtonItem
  end

  def viewDidAppear animated
    super animated
    self.tableView.editing = false
  end

  def tableView(tv, willBeginEditingRowAtIndexPath:ip)
  end

  def tableView(tv, didEndEditingRowAtIndexPath:ip)
  end

  def setEditing(editing, animated:animated)
    if editing
      ip = NSIndexPath.indexPathForRow(@data_source.size, inSection:0)
      self.tableView.insertRowsAtIndexPaths([ip], withRowAnimation:UITableViewRowAnimationTop)
    else
      ip = NSIndexPath.indexPathForRow(@data_source.size - 1, inSection:0)
      @data_source.delete_at(-1)
      self.tableView.deleteRowsAtIndexPaths([ip], withRowAnimation:UITableViewRowAnimationTop)
    end

    super(editing, animated:true)
  end

end
