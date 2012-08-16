Character = Struct.new(:name, :job)

class SampleForLocalizedIndexedCollation
  attr_accessor :data_source

  def viewDidLoad
    super

    ds_temp = 20.times.map { |i|
      name  = (i % 2) == 0 ? "Dummy" : "더미"
      dummy = Character.new(name, "아르바이트")
      dummy
    }

    ds_temp += [Character.new("김마법", "마도사"), Character.new("이강산", "백  수")]

    collation = UILocalizedIndexedCollation.currentCollation
    section_count = collation.sectionTitles.count
    sec_arr = (section_count+1).times.map { [nil] }

  end

  def sectionIndexTitlesForTableView(tv)
  end

  def tableView(tv, titleForHeaderInSection:sec)
  end

  def tableView(tv, sectionForSectionIndexTitle(title, atIndex:index)
  end

  def numberOfSectionsInTableView(tv)
  end

  def tableView(tv, numberOfRowsInSection:sec)
  end

  def tableView(tv, cellForRowAtIndexPath:ip)
  end

end
