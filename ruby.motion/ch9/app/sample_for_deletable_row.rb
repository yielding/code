class SampleForDeletableRow < UITableViewController
  def viewDidLoad
    super

    @data_source = [ 
      "item 1", "item  2", "item  3", "item  4",
      "item 5", "item  6", "item  7", "item  8", 
      "item 9", "item 10", "item 11", "item 12"]
  end

  def viewDidAppear animated
    super animated

    self.tableView.setEditing(true, animated:true)
  end

  def tableView(table, numberOfRowsInSection:section)
    @data_source.size
  end

  CELLID = "ID"
  def tableView(tv, cellForRowAtIndexPath:ip)
    cell = tv.dequeueReusableCellWithIdentifier(CELLID) || begin
      UITableViewCell.alloc.initWithStyle(UITableViewCellStyleDefault, reuseIdentifier:CELLID)
    end

    cell.textLabel.text = @data_source[ip.row]
    cell
  end

  def tableView(table, commitEditingStyle:editingStyle, forRowAtIndexPath:ip)
    if UITableViewCellEditingStyleDelete == editingStyle
      @data_source.delete_at(ip.row)
      table.deleteRowsAtIndexPaths([ip],
               withRowAnimation:UITableViewRowAnimationFade)
    end
  end
end
