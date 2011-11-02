require "rspec"

class Cell
  attr_accessor :world, :x, :y

  def initialize(world, x=0, y=0)
    @world = world
    @x = x
    @y = y
    world.cells << self
  end

  def die!
    world.cells -= [self]
  end

  def dead?
    !world.cells.include?(self)
  end

  def alive?
    world.cells.include?(self)
  end

  def neighbors
    @neighbors = []
    world.cells.each do |cell| 
      # Has a cell to the northe
      @neighbors << cell if self.x == cell.x && self.y == cell.y - 1

      # Has a cell to the north ease
      @neighbors << cell if self.x == cell.x - 1 && self.y == cell.y - 1

      # Has a cell to the west 
      @neighbors << cell if self.x == cell.x + 1 && self.y == cell.y

      # Has a cell to the east 
      @neighbors << cell if self.x == cell.x - 1 && self.y == cell.y
    end
    @neighbors
  end

  def spawn_at(x, y)
    Cell.new(world, x, y)
  end
end

class World
  attr_accessor :cells

  def initialize
    @cells = []
  end

  def tick!
    cells.each { |cell| 
      if cell.neighbors.count < 2
        cell.die!
      end
    }
  end
end

describe "game of life" do
  let(:world) { World.new }
  context "utility methods" do
    subject { Cell.new(world) }

    it "spawn relative to" do
      cell = subject.spawn_at(3, 5)
      cell.is_a?(Cell).should be_true
      cell.x.should == 3
      cell.y.should == 5
      cell.world.should == subject.world
    end

    it "detects a neighbor to the north" do 
      cell = subject.spawn_at(0, 1)
      subject.neighbors.count.should == 1
    end

    it "detects a neighbor to the left" do 
      cell = subject.spawn_at(-1, 0)
      subject.neighbors.count.should == 1
    end

    it "detects a neighbor to the north east" do
      cell = subject.spawn_at(1, 1)
      subject.neighbors.count.should == 1
    end

    it "detects a neighbor to the right" do
      cell = subject.spawn_at(1, 0)
      subject.neighbors.count.should == 1
    end

    it "dies" do
      subject.die!
      subject.world.cells.should_not include(subject)
    end
  end

  it "Rule #1: Any live cell with fewer than two live neighbors dies, ias if caused by under-population." do
    cell = Cell.new(world)
    new_cell = cell.spawn_at(2, 0)
    world.tick!
    cell.should be_dead
  end

  it "Rule #2: Any live cell with two or three live neighbours lives on to the next generation" do
    cell = Cell.new(world)
    new_cell = cell.spawn_at(1, 0)
    other_cell = cell.spawn_at(-1, 0)
    world.tick!
    cell.should be_alive
  end

  it "Rule #3: Any live cell with more than three live neighbours dies, as if by overcrowding" do
  end

  it "Rule #4: Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction" do
  end
end
