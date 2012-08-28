class Item
  attr_accessor :weapon
  attr_accessor :armor
  attr_accessor :name

  def initialize name
    @name = name 
  end

  def self.weapon_with_name name
    item = Item.new(name)
    item.weapon = true
    item
  end

  def self.armor_with_name name
    item = Item.new(name)
    item.armor = true
    item
  end
end

class SampleForSearchDisplay < UITableViewController
  def viewDidLoad
    super
    self.title = "장비검색"
    sb = UISearchBar.new
    sb.frame = [[0, 0], [self.tableView.bounds.size.width, 0]]
    sb.sizeToFit
    sb.scopeButtonTitles = ["모두", "무기", "방어구"]
    sb.showsScopeBar     = true
    self.tableView.tableHeaderView = sb

    @search_display = UISearchDisplayController.alloc.initWithSearchBar(sb, 
                      contentsController:self)
    @search_display.delegate = self
    @search_display.searchResultsDataSource = self
    @search_display.searchResultsDelegate   = self

    weapons = ["세라믹 양날검", "세라믹 칼", "성스러운 자"]
    armors  = ["세라믹 갑옷", "사라믹 방패", "세라믹 투구", "마법의 망또", "크르비스 슈트"]
    
    @data_source  = weapons.map { |name| Item.weapon_with_name(name) }
    @data_source +=  armors.map { |name| Item.armor_with_name(name)  }
    @data_source.sort_by { |item| item.name }

    @search_result = []
  end

  def searchDisplayController(ctrl, shouldReloadTableForSearchString:ss)
    @search_result = []
    @data_source.each do |item|
      rng_str = item.name.rangeOfString(ss)
      @search_result << item unless rng_str.location == NSNotFound
    end
    true
  end

  def searchDisplayController(ctrl, shouldReloadTableForSearchScope:search_opt)
    @search_result = []
    ss = ctrl.searchBar.text
    @data_source.each do |item| 
      unless item.name.rangeOfString(ss).location == NSNotFound
        if search_opt == 0
          @search_result << item
        elsif search_opt == 1
          @search_result << item if item.weapon
        else
          @search_result << item if item.armor
        end
      end
    end
    true
  end

  def tableView(tv, numberOfRowsInSection:sec)
    if tv == self.searchDisplayController.searchResultsTableView
      return @search_result.size
    else
      return @data_source.size
    end
  end

  CELL_ID = "id"
  def tableView(tv, cellForRowAtIndexPath:ip)
    cell = tv.dequeueReusableCellWithIdentifier(CELL_ID)
    if cell.nil?
      cell = UITableViewCell.alloc.initWithStyle(UITableViewCellStyleDefault, reuseIdentifier:CELL_ID)
    end

    if tv == self.searchDisplayController.searchResultsTableView
      cell.textLabel.text = @search_result[ip.row].name
    else
      cell.textLabel.text = @data_source[ip.row].name
    end

    cell
  end
end
