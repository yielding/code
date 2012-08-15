class CellWithImageView < UITableViewCell
  attr_accessor :image_view

  def initWithReuseIdentifier(id)
    self.initWithStyle(UITableViewCellStyleDefault, reuseIdentifier:id)
  end

  def initWithStyle(style, reuseIdentifier:id)
    if super
      @image_view = UIImageView.alloc.init
      @image_view.autoresizingMask = 
        UIViewAutoresizingFlexibleLeftMargin or UIViewAutoresizingFlexibleRightMargin or
        UIViewAutoresizingFlexibleTopMargin  or UIViewAutoresizingBottomMargin
      self.addSubview(@image_view)
    end
    self
  end

  def layoutSubviews
    super
    @image_view.sizeToFit
    new_center = self.contentView.center
    new_center.x += 80
    @image_view.center = new_center
  end
end

class CellWithSwitch < UITableViewCell
  attr_accessor :the_switch

  def initWithReuseIdentifier(id)
    self.initWithStyle(UITableViewCellStyleDefault, reuseIdentifier:id)
  end

  def initWithStyle(style, reuseIdentifier:id)
    if super
      @the_switch = UISwitch.alloc.init
      @the_switch.on = true
      @the_switch.autoresizingMask = 
        UIViewAutoresizingFlexibleLeftMargin or UIViewAutoresizingFlexibleRightMargin or
        UIViewAutoresizingFlexibleTopMargin or UIViewAutoresizingBottomMargin

      self.addSubview(@the_switch)
    end
    self
  end

  def layoutSubviews
    super
    new_center = self.contentView.center
    new_center.x += 80
    @the_switch.center = self.contentView.center
  end
end

class CellWithSlider < UITableViewCell
  attr_accessor :slider
  attr_accessor :row
  attr_accessor :delegate

  def initWithReuseIdentifier(id)
    self.initWithStyle(UITableViewCellStyleDefault, reuseIdentifier:id)
  end

  def initWithStyle(style, reuseIdentifier:id)
    if super
      @slider = UISlider.alloc.init
      @slider.frame = [[0, 0], [160, 20]]
      @slider.autoresizingMask = 
        UIViewAutoresizingFlexibleLeftMargin or UIViewAutoresizingFlexibleRightMargin or
        UIViewAutoresizingFlexibleTopMargin  or UIViewAutoresizingBottomMargin

      @slider.addTarget(self, action: "sliderValueDidChange:", 
                              forControlEvents:UIControlEventValueChanged)
      self.addSubview(@slider)
    end
    self
  end

  def layoutSubviews
    super
    new_center = self.contentView.center
    new_center.x += 50
    @slider.center = new_center
  end

  def sliderValueDidChange(s)
    self.delegate.slider(s, valueDidChange:s.value, forRow:self.row)
  end
end

class SampleForCustomizedCell < UITableViewController
  attr_accessor :sections
  attr_accessor :data_source
  attr_accessor :slider_values

  CELL_IDS = ["image-cell", "switch-cell", "slider-cell"]

  def init
    self.initWithStyle(UITableViewStyleGrouped)
  end

  def initWithStyle(style)
    super
    self
  end

  def viewDidLoad
    super
    @sections = ["이름", "필살기", "힘"]
    @data_source = { @sections[0] => [ "홍길동"],
                     @sections[1] => [ "쌍수검"],
                     @sections[2] => [ "공격력", "방어력"] }
    @slider_values = [0.9, 0.8]
  end

  def numberOfSectionsInTableView(tv)
    @sections.size
  end

  def tableView(tv, numberOfRowsInSection:sec)
    @data_source.to_a[sec][1].size
  end

  def tableView(tv, cellForRowAtIndexPath:ip)
    id = CELL_IDS[ip.section]
    cell = tv.dequeueReusableCellWithIdentifier(id)
    if cell.nil?
       case ip.section
       when 0; cell = CellWithImageView.alloc.initWithReuseIdentifier(id)
       when 1; cell = CellWithSwitch.alloc.initWithReuseIdentifier(id)
       else    cell = CellWithSlider.alloc.initWithReuseIdentifier(id)
               cell.delegate = self
       end
    end

    case ip.section
    when 0; cell.image_view.image = UIImage.imageNamed("Samurai.png")
    when 2; cell.slider.value = slider_values[ip.row].to_f
            cell.row = ip.row
    end

    cell.textLabel.text = @data_source.to_a[ip.section][1][ip.row]
    cell
  end

  def tableView(tv, heightForRowAtIndexPath:ip)
    ip.section == 0 ? 100.0 : 44.0
  end

  def slider(s, valueDidChange:v, forRow:row)
    @slider_values[row] = v
  end
end
