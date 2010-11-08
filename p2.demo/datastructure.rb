require 'byte_buffer'

class Array
  def load_to(buffer)
    buffer.set_int4(self.size)
    self.each { |f| f.load_to(buffer) }
  end
  
  def load_from(buffer, target)
    count = buffer.get_int4
    1.upto(count) {
      f = target.new
      f.load_from(buffer, target)
      self.push(f)
    }
  end

  def byte_count
    total = 4
    self.each { |f| total += f.byte_count }
    total
  end
  
end

# module PCLRuby
class CharField
  attr_accessor :name, :value
  
  def initialize(name=nil, value=nil)
    @name  = name
    @value = value
  end
  
  def byte_count
    @name.length + @value.length + 2
  end
  
  def deep_copy
    CharField.new(@name, @value)
  end
  
  def load_to(buffer)
    buffer.set_string(name)
    buffer.set_string(value)
  end
  
  def load_from(buffer, target)
    @name  = buffer.get_string
    @value = buffer.get_string 
  end
  
  def to_s
    "#{@name}[0]#{@value}[0]"
  end
end

class BinaryField
  attr_accessor :name, :value
  
  def initialize(name=nil, value=nil)
    @name = name
    @value = value
  end
  
  def byte_count
    @name.length + 1 + 4 + @value.length
  end
  
  def deep_copy
    BinaryField.new(@name, @value)
  end
  
  def size
    @value.size
  end
  
  def load_to(buffer)
    buffer.set_string(name)
    buffer.set_int4(@value.size)
    buffer.set_binary(@value)
  end
  
  def load_from(buffer, target)
    @name  = buffer.get_string
    size   = buffer.get_int4
    @value = Array.new(buffer.get_binary(size))
  end
  
  def to_s
    arr = "[" + @value.join(", ") +"]"
    size = sprintf("[%-04d]",@value.size)
    "#{@name}[0]#{size}#{arr}"
  end
end

class Row
  attr_accessor :cfields, :bfields
  
  def initialize
    @cfields = []
    @bfields = []
  end
  
  def add_field(name, value)
    if value.class == Array
      add_bfield(name, value)
    else
      add_cfield(name, value)
    end
  end
  
  def add_field2(field)
    if field.value.class == Array
      add_bfield2(field)
    else
      add_cfield2(field)
    end
  end
  
  def find_field(name)
    res = @cfields.find { |cf| cf.name == name }
    return res != nil ? res : @bfields.find { |bf| bf.name == name }
  end
    
  def field(name)
    return find_field(name)
  end

  def update_field(name, value)
    field = self.find_field(name)
    return false if field == nil
    field.value = value
    return true
  end
    
  def deep_copy
    row = Row.new
    @cfields.each { |f| row.cfields.push(CharField.new(f.name, f.value))   }
    @bfields.each { |f| row.bfields.push(BinaryField.new(f.name, f.value)) }
    row
  end
  
  def byte_count
    @cfields.byte_count + @bfields.byte_count
  end
  
  def load_to(buffer)
    @cfields.load_to(buffer)
    @bfields.load_to(buffer)
  end
  
  def load_from(buffer, target)
    @cfields.load_from(buffer, CharField)
    @bfields.load_from(buffer, BinaryField)
  end
    
  def to_s
    res  = "<" + sprintf("[%-04d]",@cfields.size)
    @cfields.each { |f| res += f.to_s }
    res += " | "
    res += sprintf("[%-04d]",@bfields.size)
    @bfields.each { |f| res += f.to_s }
    res += ">"
    res
  end  
  
  private
  def add_cfield2(cf)
    @cfields.push(cf.deep_copy)
  end
  
  def add_cfield(name, value)
    @cfields.push(CharField.new(name, value))
  end

  def add_bfield2(bf)
    @bfields.push(bf.deep_copy)
  end
  
  def add_bfield(name, value)
    @bfields.push(BinaryField.new(name, value))
  end
end

class Structure < Row
  attr_accessor :name
  
  def initialize(name="st")
    super()
    @name = name
  end
  
  def deep_copy
    st = Structure.new(@name)
    @cfields.each { |f| st.cfields.push(CharField.new(f.name, f.value))   }
    @bfields.each { |f| st.bfields.push(BinaryField.new(f.name, f.value)) }
    st
  end

  def byte_count
   @name.length + 1 + @cfields.byte_count + @bfields.byte_count
  end
  
  def load_to(buffer)
    buffer.set_string(@name)
    super(buffer)
  end
  
  def load_from(buffer)
    @name = buffer.get_string
    super(buffer)
  end
    
  def to_s
    res  = "<" + "#{@name}[0]"
    res += sprintf("[%-04d]",@cfields.size)
    @cfields.each { |f| res += f.to_s }
    res += " | "
    res += sprintf("[%-04d]",@bfields.size)
    @bfields.each { |f| res += f.to_s }
    res += ">"
    res
  end  
end

class Table
  attr_accessor :name, :rows
  
  def initialize(name="result")
    @name = name
    @rows = []
  end
  
  def add_row(row)
    @rows.push(row.deep_copy)
  end
  
  def update_field(index, name, value)
    return false if index >= @rows.size
    return @rows[index].update_field(name, value)
  end
  
  def deep_copy
    table = Table.new(@name)
    @rows.each { |row| table.rows.push(row.deep_copy) }
    table
  end

  def row_count
    @rows.size
  end
  
  def byte_count
    @name.length + 1 + @rows.byte_count
  end

  def load_to(buffer)
    buffer.set_string(@name)
    @rows.load_to(buffer)
  end
  
  def load_from(buffer, target)
    @name = buffer.get_string
    @rows.load_from(buffer, Row)
  end
  
  def to_s
    res  = "#{@name}[0]"
    res += sprintf("[%-04d]",@rows.size)
    @rows.each { |row| res += row.to_s }  
    res
  end
end

class ParameterList
  attr_accessor :cfields, :bfields, :structures, :tables
  
  def initialize
    @cfields = []
    @bfields = []
    @structures = []
    @tables = []
  end
  
  def add_field(name, value)
    return value.class == Array ? add_bfield(name, value) : add_cfield(name, value)
  end
  
  def add_field2(field)
    if field.value.class == Array 
      add_bfield2(field)
    else
      add_cfield2(field)
    end
  end
  
  def add_strucure(st)
    @structures.push(st.deep_copy)
  end
  
  def add_table(table)
    @tables.push(table.deep_copy)
  end
  
  def remove_field(name)
    @cfields.delete_if { |f| f.name == name }
    @bfields.delete_if { |f| f.name == name }
  end
  
  def get_table(name)
    @tables.find(name)
  end
  
  def get_structure(name)
    @structures.find(name)
  end
  
  def get_field(name)
    res = @cfields.find(name)
    return res != nil ? res : @bfields.find(name)
  end
  
  def field_count
    @cfields.size + @bfields.size
  end
  
  def cfield_count
    @cfields.size
  end
  
  def bfield_count
    @bfields.size
  end
  
  def structure_count
    @structures.size
  end
  
  def table_count
    @tables.size
  end
  
  def byte_count
    @cfields.byte_count + @bfields.byte_count + @structures.byte_count + @tables.byte_count
  end
  
  def load_to(buffer)
    @cfields.load_to(buffer)
    @bfields.load_to(buffer)
    @structures.load_to(buffer)
    @tables.load_to(buffer)
  end
  
  def load_from(buffer)
    @cfields.load_from(buffer, CharField)
    @bfields.load_from(buffer, BinaryField)
    @structures.load_from(buffer, Structure)
    @tables.load_from(buffer, Table)
  end

  private
  def add_cfield(name, value)
    @cfields.push(CharField.new(name, value))
  end
  
  def add_cfield2(cf)
    @cfields.push(CharField.new(cf.name, cf.value))
  end
  
  def add_bfield(name, value)
    @bfields.push(BinaryField.new(name, value))
  end
  
  def add_bfield2(bf)
    @bfields.push(BinaryField.new(bf.name, bf.value))
  end
  
end
