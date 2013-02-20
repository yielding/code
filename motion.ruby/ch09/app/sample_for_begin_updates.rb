class SampleForBeginUpdates < UITableViewController
  def viewDidLoad
    super

    @data_source = ["ITEM X1", "ITEM 1", "ITEM X2", "ITEM 3"]
    bar_btn = UIBarButtonItem.alloc.initWithTitle("일괄처리 개시",
                style:UIBarButtonItemStyleBordered,
               target:self,
               action:"batchButtonDidPush:")
    self.navigationItem.rightBarButtonItem = bar_btn
  end

  def batchButtonDidPush sender
    @data_source = ["ITEM 1", "ITEM 2", "ITEM 3"]

    to_del = [NSIndexPath.indexPathForRow(0, inSection:0),
              NSIndexPath.indexPathForRow(2, inSection:0)]
    to_ins = [NSIndexPath.indexPathForRow(1, inSection:0)]

    tableView.beginUpdates
    tableView.deleteRowsAtIndexPaths(to_del, 
              withRowAnimation:UITableViewRowAnimationFade)
    tableView.insertRowsAtIndexPaths(to_ins,
              withRowAnimation:UITableViewRowAnimationFade)
    tableView.endUpdates

    if sender.class == UIBarButtonItem.class
      sender.enabled = false
    end
  end

  def tableView(tv, numberOfRowsInSection:sec)
    @data_source.size
  end

  CELLID = "id"
  def tableView(tv, cellForRowAtIndexPath:ip)
    cell = tv.dequeueReusableCellWithIdentifier(CELLID) || begin
      UITableViewCell.alloc.initWithStyle(UITableViewCellStyleDefault, 
                                          reuseIdentifier:CELLID)
    end

    cell.textLabel.text = @data_source[ip.row]
    cell
  end
end
