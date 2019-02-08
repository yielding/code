#!/usr/bin/env ruby

require 'test/unit'
require 'dim'
require 'pp'

class ConsoleAppender
end

class Logger
  attr_accessor :appender
end

class MockDB
end

class RealDB
  attr_accessor :username, :password
  def initialize(username, password)
    @username, @password = username, password
  end
end

class App
  attr_accessor :logger, :db
  def initialize(logger=nil)
    @logger = logger
  end
end

class TestDim < Test::Unit::TestCase
  def setup
    @c = DIM::Container.new
  end

  def test_simple_object
    @c.register(:app) { App.new }
    assert @c.app.kind_of?(App), "Should get an App"
  end

  def test_get_same_object
    @c.register(:app) { App.new }
    app = @c.app
    #assert_equal app, @c.app
  end

  def test_constructor_based_injection
    @c.register(:app) do |c| 
      App.new(c.logger) 
    end
    @c.register(:logger) { Logger.new }
    app = @c.app
    assert app.kind_of?(App)
    assert_equal @c.logger, app.logger
  end

  def test_setter_based_injection
    sp = @c.register(:app) do |c|
      app = App.new
      app.db = c.database
      app
    end
    @c.register(:logger)   { Logger.new }
    @c.register(:database) { MockDB.new }
    app = @c.app
    assert app.kind_of?(App)
    assert_equal nil, app.logger
  end

  def test_combined_injection
    sp = @c.register(:app) do |c|
      app = App.new(c.logger)
      app.db = c.database
      app
    end
    @c.register(:logger)   { Logger.new }
    @c.register(:database) { MockDB.new }
    app = @c.app
    assert app.kind_of?(App)
    assert_equal @c.logger, app.logger
  end

  def test_multiple_levels
    @c.register(:app) do |c|
      app = App.new(c.logger)
      app.db = c.database
      app
    end
    @c.register(:logger) do |c|
      log = Logger.new
      log.appender = c.logger_appender
      log
    end
    @c.register(:logger_appender) { ConsoleAppender.new }
    @c.register(:database) { MockDB.new }
    app = @c.app
    logger = app.logger
    assert_equal @c.logger_appender, logger.appender
  end

  def test_literals
    @c.register(:database) do |c| RealDB.new(c.username, c.userpassword) end
    @c.register(:username)     { "jim" }
    @c.register(:userpassword) { "secret" }

    db = @c.database
    assert_equal "jim", db.username
    assert_equal "secret", db.password
  end

  def test_inline_literals
    @c.register(:database) { RealDB.new("jim", "secret") }
    db = @c.database
    assert_equal "jim", db.username
    assert_equal "secret", db.password
  end

  def test_missing_service_point_error
    ex = assert_raises(DIM::MissingServiceError) do
      @c.not_here
    end
    assert_match /unknown service/i, ex.message
    assert_match /not_here/, ex.message
  end

  def test_duplicate_service_name
    @c.register(:app) { App.new }
    ex = assert_raises(DIM::DuplicateServiceError) do
      @c.register(:app) { App.new }
    end
    assert_match /duplicate service/i, ex.message
    assert_match /app/, ex.message
  end

  def test_parent_container
    @c.register(:gene) { |c| c.actual_gene }
    son = DIM::Container.new(@c)
    son.register(:actual_gene) { |c| "Y" }

    daughter = DIM::Container.new(@c)
    daughter.register(:actual_gene) { |c| "X" }

    assert_raises(DIM::MissingServiceError) do
      @c.gene
    end
    assert_equal "X", daughter.gene
    assert_equal "Y", son.gene
  end

  def test_override_parent
    @c.register(:thing) { "THING" }
    child = DIM::Container.new(@c)
    child.register(:thing) { "NEWTHING" }
    assert_equal "NEWTHING", child.thing
    assert_equal "THING", @c.thing
  end

  def test_indirect_override_parent
    @c.register(:thing) { |c| c.real_thing }
    @c.register(:real_thing) { "THING" }
    child = DIM::Container.new(@c)
    child.register(:real_thing) { "NEWTHING" }
    assert_equal "NEWTHING", child.thing
    assert_equal "THING", @c.thing
  end

  def test_indirect_non_override_parent
    @c.register(:thing) { |c| @c.real_thing }
    @c.register(:real_thing) { "THING" }
    child = DIM::Container.new(@c)
    child.register(:real_thing) { "NEWTHING" }
    assert_equal "THING", child.thing
    assert_equal "THING", @c.thing
  end
end
