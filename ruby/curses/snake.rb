require 'curses'
include Curses
require 'time'
require 'colored'

class Snake

	def check_wall_collision
	end

	def check_self_collision
	end

	def check_food_eaten
	end

	def draw_snake
	end

	def make_food
	end
end

class GamePlay

	def change_direction
	end

	def end_of_game
	end

	def speed_of_game
	end
end

def change_of_dir
	case getch
	when ?Q, ?q
		exit
	when ?W, ?w
		@dir = :up if @dir != :down
	when ?S, ?s
		@dir = :down if @dir != :up
	when ?D, ?d
		@dir = :right if @dir != :left
	when ?A, ?a
		@dir = :left if @dir != :right
	when ?P, ?p
		@pause = @pause ? false : true
	end
end

def end_game
	puts "You LOST".red
	exit
end

def make_food(max_h, max_w)
	@food_y = rand(2..max_w-2)
	@food_x = rand(1..max_h-2)
end

init_screen
cbreak
noecho						#does not show input of getch
stdscr.nodelay = 1 			#the getch doesnt system_pause while waiting for instructions
curs_set(0)					#the cursor is invisible.


#starting position
title = "Kirka's Snake"
pos_y = [5,4,3,2,1]
pos_x = [1,1,1,1,1]
@dir = :right
@pause = false
snake_len = 3
width = cols
height = lines
game_speed = 0.2
make_food(height, width)
start_time = Time.now.to_i
speed_incremented = false
display_speed = 0
game_score = 0
win = Window.new(height, width, 0, 0) #set the playfield the size of current terminal window

begin
	loop do

		change_of_dir

		if @pause
			sleep(0.5)
			next
		end

		time_offset = Time.now.to_i - start_time

		win.box("|", "-")

		win.setpos(@food_x, @food_y)
		win.addstr("#")

		win.setpos(0,3)
		win.addstr("Snake Length: " + snake_len.to_s)

		win.setpos(0,width/2-title.length/2)
		win.addstr(title)

		win.setpos(0,width-12)
		win.addstr("Time: " + time_offset.to_s)

		win.setpos(height-1,3)
		win.addstr("Speed: " + display_speed.to_s)

		win.setpos(height-1,width-12)
		win.addstr("Score: " + (game_score-(time_offset)/10.round(0)).to_s)

		#change direction of movement
		case @dir
		when :up    then pos_x[0] -= 1
		when :down  then pos_x[0] += 1
		when :left  then pos_y[0] -= 1
		when :right then pos_y[0] += 1
		end

		#remember the tail position during movement
		t = snake_len+1
		while t > 0 do
			pos_x[t] = pos_x[t-1]
			pos_y[t] = pos_y[t-1]
			t -= 1
		end 

		#draw the snake and its tail
		for t in 0..snake_len+1
			setpos(pos_x[t],pos_y[t])
			addstr(t == 1 ? "*" : "+")
		end

		#set speed of play, increment it automatically
		if ((snake_len % 10 == 0) or (time_offset%60 == 0))
			if speed_incremented == false
				game_speed -= (game_speed*0.10) unless game_speed < 0.05
				speed_incremented = true
				display_speed += 1
			end
		else
			speed_incremented = false
		end

		sleep( (@dir == :left or @dir == :right) ? game_speed/2 : game_speed)

		#check collision with border
		if pos_y[0] == cols-1 or pos_y[0] == 0 or pos_x[0] == lines-1 or pos_x[0] == 0
			end_game
		end

		#check collision with self
		for i in 2..snake_len
			if pos_y[0] == pos_y[i] and pos_x[0] == pos_x[i]
				end_game
			end
		end

		#check if ate food
		if pos_y[0] == @food_y and pos_x[0] == @food_x
			make_food(height,width)
			snake_len += 1
			game_score += 1*display_speed
		end
		win.refresh
		win.clear
	end
ensure
	close_screen
end
