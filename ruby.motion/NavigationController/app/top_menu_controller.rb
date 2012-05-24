class TopMenuController < UITableViewController
  def init
    if super.initWithStyle(UITableViewStylePlain)  # =====> initWithStyle()
      self.title = "최상위 메뉴"
      @items = ["ViewController1", "ViewController2"]
    end
    self
  end

  def viewDidLoad
    view.dataSource = view.delegate = self
    navigationItem.title = "Navigation View"
  end

  def tableView(tableView, numberOfRowsInSection:section)
    @items.size
  end

  CELLID = "simple-cell"
  def tableView(tableView, cellForRowAtIndexPath:indexPath)
    cell = tableView.dequeueReusableCellWithIdentifier(CELLID) || begin
      zero = [[0, 0], [0, 0]]
      cell = UITableViewCell.alloc.initWithFrame(zero, reuseIdentifier:CELLID) 
      cell
    end

    cell.textLabel.text = @items[indexPath.row]
    cell
  end

  def tableView(tableView, didSelectRowAtIndexPath:indexPath)
    vc = Object.const_get(@items[indexPath.row]).alloc.init
    self.navigationController.pushViewController(vc, animated:true) unless vc.nil?
  end
end
