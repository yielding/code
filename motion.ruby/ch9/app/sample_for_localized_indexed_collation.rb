class Character
  attr_accessor :name
  attr_accessor :job

  def initialize(name, job)
    @name = name
    @job  = job
  end
end

class SampleForLocalizedIndexedCollation < UITableViewController
  attr_accessor :data_source

  def viewDidLoad
    super

    ds_temp = 20.times.map { |i|
      name  = (i % 2) == 0 ? "Dummy" : "더미"
      ch = Character.new(name, "아르바이트")
      ch
    }

    ds_temp += [Character.new("김마법", "마도사"), Character.new("이강산", "백  수")]

    collation = UILocalizedIndexedCollation.currentCollation
    section_count = collation.sectionTitles.count
    sec_arrs = (section_count+1).times.map { [] }

    ds_temp.each { |ch| 
      sect = collation.sectionForObject(ch, collationStringSelector:"name")
      sec_arrs[sect] << ch
    }

    @data_source = []
    sec_arrs.each { |arr| 
      sorted = collation.sortedArrayFromArray(arr, collationStringSelector:"name")
      @data_source << sorted
    }
  end

  def sectionIndexTitlesForTableView(tv)
    UILocalizedIndexedCollation.currentCollation.sectionIndexTitles
  end

  def tableView(tv, titleForHeaderInSection:sec)
    return nil if @data_source[sec].size < 1
    cl = UILocalizedIndexedCollation.currentCollation
    cl.sectionIndexTitles[sec]
  end

  def tableView(tv, sectionForSectionIndexTitle:title, atIndex:index)
    cl = UILocalizedIndexedCollation.currentCollation
    cl.sectionForSectionIndexTitleAtIndex(index)
  end

  def numberOfSectionsInTableView(tv)
    @data_source.size
  end

  def tableView(tv, numberOfRowsInSection:sec)
    @data_source[sec].size
  end

  CELL_ID = "basis-cell"
  def tableView(tv, cellForRowAtIndexPath:ip)
    cell = tv.dequeueReusableCellWithIdentifier(CELL_ID) || begin
      UITableViewCell.alloc.initWithStyle(UITableViewCellStyleDefault, reuseIdentifier:CELL_ID)
    end

    ch_ = @data_source[ip.section][ip.row]
    cell.textLabel.text = ch_.name
    cell
  end
end
