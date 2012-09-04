class SampleForSearchBar < UITableViewController
  def viewDidLoad
    super 

    @sb = UISearchBar.alloc.initWithFrame([[0, 0], [self.tableView.bounds.size.width, 0]])
    @sb.delegate = self
    @sb.barStyle = UIBarStyleBlack
    @sb.sizeToFit

    @db = 64.times.map { |i| i.to_s }
    @ds = 64.times.map { |i| i.to_s }

    self.tableView.tableHeaderView = @sb
  end

  def searchBarSearchButtonClicked(sb)
    @ds = []
    @db.each {|data|
      @ds << data if data.match(sb.text)
    }

    self.tableView.reloadData
    sb.resignFirstResponder
  end

  def tableView(tv, numberOfRowsInSection:sec)
    @ds.size
  end

  CELL_ID='xxx'
  def tableView(tv, cellForRowAtIndexPath:ip)
    cell = tv.dequeueReusableCellWithIdentifier(CELL_ID) || begin
      UITableViewCell.alloc.initWithStyle(UITableViewCellStyleDefault, reuseIdentifier:CELL_ID)
    end
    cell.textLabel.text = @ds[ip.row]

    cell
  end


end
