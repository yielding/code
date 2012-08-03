class SampleForSimpleTable < UITableViewController
  def viewDidLoad
    super

    self.title = "SimpleTable"
    @data_source = [
      "ITEM  1", "ITEM  2", "ITEM  3", "ITEM  4", "ITEM  5", "ITEM  6",
      "ITEM  7", "ITEM  8", "ITEM  9", "ITEM 10", "ITEM 11", "ITEM 12",
      "ITEM 13", "ITEM 14", "ITEM 15", "ITEM 16", "ITEM 17", "ITEM 18",
      ]
  end

  def tableView(tv, numberOfRowsInSection:sec)
    @data_source.size
  end

  CELLID = "basis-cell"
  def tableView(tv, cellForRowAtIndexPath:ip)
    cell = tv.dequeueReusableCellWithIdentifier(CELLID) || begin
      UITableViewCell.alloc.initWithStyle(UITableViewCellStyleDefault, reuseIdentifier:CELLID)
    end

    cell.textLabel.text = @data_source[ip.row]
    cell
  end

  def tableView(tv, didSelectRowAtIndexPath:ip)
    message = @data_source[ip.row]
    alert   = UIAlertView.alloc.init 
    alert.message = message
    alert.addButtonWithTitle "OK"
    alert.show
  end
end

