class SampleForInsertableRow < UITableViewController
  def viewDidLoad
    super
    @data_source = ["ITEM 1", "ITEM 2", "ITEM 3", "신규추가"]
  end

  def viewDidAppear animated
    super animated
    self.tableView.setEditing(true, animated:true)
  end

  def tableView(tv, numberOfRowsInSection:sec)
    @data_source.size
  end

  CELLID = "id"
  def tableView(tv, cellForRowAtIndexPath:ip)
    cell = tv.dequeueReusableCellWithIdentifier(CELLID) || begin 
      UITableViewCell.alloc.initWithStyle(UITableViewCellStyleDefault, reuseIdentifier:CELLID)
    end

    cell.textLabel.text = @data_source[ip.row]
    cell
  end

  def tableView(tv, editingStyleForRowAtIndexPath:ip)
    if tv.editing? and (@data_source.size <= ip.row + 1)
      return UITableViewCellEditingStyleInsert 
    end

    UITableViewCellEditingStyleDelete 
  end

  def tableView(tv, commitEditingStyle:editingStyle, forRowAtIndexPath:ip)
    case editingStyle 
    when UITableViewCellEditingStyleDelete
      @data_source.delete_at ip.row 
      tv.deleteRowsAtIndexPaths([ip], withRowAnimation:UITableViewRowAnimationLeft)
    when UITableViewCellEditingStyleInsert
      @data_source.insert(@data_source.size - 1, "NEW ITEM")
      tv.insertRowsAtIndexPaths([ip], withRowAnimation:UITableViewRowAnimationBottom)
    end
  end
end
