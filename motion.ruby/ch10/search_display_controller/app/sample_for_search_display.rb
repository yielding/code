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
    p "11111"
    sb = UISearchBar.alloc.initWithFrame(
            CGRectMake(0, 0, self.tableView.bounds.size.width, 0))
    sb.delegate = self
    sb.showsCancelButton = true
    sb.sizeToFit
    sb.scopeButtonTitles = ["모두", "무기", "방어구"]
    sb.showsScopeBar     = true
    #self.tableView.tableHeaderView = sb

    p "22222"
    @sd = UISearchDisplayController.alloc.initWithSearchBar(sb, contentsController:self)
    @sd.delegate = self
    @sd.searchResultsDataSource = self
    @sd.searchResultsDelegate   = self

    weapons = ["세라믹 양날검", "세라믹 칼", "성스러운 자"]
    armors  = ["세라믹 갑옷", "사라믹 방패", "세라믹 투구", "마법의 망또", "크르비스 슈트"]
    
    p "33333"
    @data_source  = weapons.map { |name| Item.weapon_with_name(name) }
    @data_source +=  armors.map { |name| Item.armor_with_name(name)  }
    @data_source.sort_by { |item| item.name }

    p "44444#{@data_source}"

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

  def searchDisplayController(ctrl, shouldReloadTableForSearchScope:so) # search option
    @search_result = []
    ss = ctrl.searchBar.text
    @data_source.each do |item| 
      unless item.name.rangeOfString(ss).location == NSNotFound
        if so == 0
          @search_result << item
        elsif so == 1
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
      cell = UITableViewCell.alloc.initWithStyle(UITableViewCellStyleDefault, 
                                                 reuseIdentifier:CELL_ID)
    end

    if tv == self.searchDisplayController.searchResultsTableView
       cell.textLabel.text = @search_result[ip.row].name
    else
       cell.textLabel.text = @data_source[ip.row].name
    end

    return cell
  end
end
