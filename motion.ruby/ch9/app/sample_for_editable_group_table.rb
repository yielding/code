class SampleForEditableGroupTable < UITableViewController
  def init
    self.initWithStyle(UITableViewStyleGrouped) if super
    self
  end

  def viewDidLoad
    super

    @keys = ["READ ONLY", "SECTION 2"]
    @data_source = {
      @keys[0] => ["ITEM 1", "ITEM 2", "ITEM 3"],
      @keys[1] => ["ITEM 1", "ITEM 2"]
    }
    
    self.navigationItem.rightBarButtonItem = self.editButtonItem
  end

  def tableView(table, numberOfRowsInSection:section)
    @data_source.to_a[section][1].size
  end

  CELLID = "id"
  def tableView(tv, cellForRowAtIndexPath:ip)
    cell = tv.dequeueReusableCellWithIdentifier(CELLID) || begin
      UITableViewCell.alloc.initWithStyle(UITableViewCellStyleDefault, reuseIdentifier:CELLID)
    end

    key  = @data_source.to_a[ip.section][0]
    text = @data_source[key][ip.row]
    cell.textLabel.text = text
    cell
  end


  def numberOfSectionsInTableView(tv)
    @keys.size
  end

  def tableView(tv, titleForHeaderInSection:section)
    @keys[section]
  end

  def tableView(tv, commitEditingStyle:editingStyle, forRowAtIndexPath:ip)
    if UITableViewCellEditingStyleDelete == editingStyle
      data = @data_source.to_a[ip.section]
      data.delete_at(ip.row)
      # TODO
      # [dataSource_ replaceObjectAtIndex:indexPath.section withObject:datas];
      # @data_source = ......;
      tv.deleteRowsAtIndexPaths([ip], withRowAnimation:UITableViewRowAnimationLeft)
    end
  end

  def tableView(tv, canEditRowAtIndexPath:ip)
    ip.section == 1
  end

  def tableView(tv, moveRowAtIndexPath:from, toIndexPath:to)
    # TODO
    f = from.row
    t = to.row

    while f < t
      @data_source.insert(f, @data_source.delete_at(f+1))
      f += 1
    end

    while f > t
      @data_source.exchangeObjectAtIndex(f, withObjectAtIndex:f-1)
      f -= 1
    end
  end

  def tableView(tv, willBeginEditingRowAtIndexPath:ip)
  end

  def tableView(tv, didEndEditingRowAtIndexPath:ip)
  end

end
