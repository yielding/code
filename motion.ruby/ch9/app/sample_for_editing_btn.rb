class SampleForEditingButton < SampleForMovableRow
  def viewDidLoad
    super
    @data_source = ["ITEM 1", "ITEM 2", "ITEM 3"]
    navigationItem.rightBarButtonItem = self.editButtonItem
  end

  def viewDidAppear animated
    super animated
    tableView.editing = false
  end

  def tableView(tv, willBeginEditingRowAtIndexPath:ip)
  end

  def tableView(tv, didEndEditingRowAtIndexPath:ip)
  end

  def setEditing(editing, animated:ani)
    if editing
      ip = NSIndexPath.indexPathForRow(@data_source.size, inSection:0)
      @data_source << "신규"
      tableView.insertRowsAtIndexPaths([ip], withRowAnimation:UITableViewRowAnimationTop)
    else
      ip = NSIndexPath.indexPathForRow(@data_source.size - 1, inSection:0)
      @data_source.delete_at(-1)
      tableView.deleteRowsAtIndexPaths([ip], withRowAnimation:UITableViewRowAnimationTop)
    end

    super(editing, animated:true)
  end

end
