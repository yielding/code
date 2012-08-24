class SampleForSearchDisplay < UITableViewController
  def viewDidLoad
    super
    self.title = "장비검색"
    sb = UISearchBar.new
    sb.frame = [[0, 0], [self.tableView.bounds.size.width, 0]]
    sb.sizeToFit
    sb.scopeButtonTitles = ["모두", "무기", "방어구"]
    sb.showsScopeBar = true
    self.tableView.tableHeaderView = sb
  end
end
