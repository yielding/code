#!/usr/local/bin/macruby

framework 'Foundation'

class Player
  attr_accessor :x, :y

  def initialize
    @x = @y = 0
  end

  def undo_manager
    @manager ||= NSUndoManager.alloc.init
  end

  def left
    undo_manager.prepareWithInvocationTarget(self).right
    @x -= 1
  end

  def right
    undo_manager.prepareWithInvocationTarget(self).left
    @x += 1
  end
end

lara = Player.new

p lara
p lara.undo_manager.canUndo
p lara.left
p lara.undo_manager.canUndo
p lara.x
p lara.undo_manager.undo
p lara.x
p lara.undo_manager.canRedo
p lara.undo_manager.redo
p lara.x

