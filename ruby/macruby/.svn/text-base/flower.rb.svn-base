require 'hotcocoa/graphics'
include HotCocoa
include Graphics

# initialize canvas
canvas = Canvas.new :type => :image, :filename => 'particles.png', :size => [400,400]
canvas.background(Color.black)

# load images and grab colors
img = Image.new('Ballerina3.jpg').saturation(1.9)
redcolors = img.colors(100)
img = Image.new('Hot_Air_Balloons_H.jpg').saturation(1.9)
bluecolors = img.colors(100)

# create flower head shape
head = Path.new.oval(0,0,10,10,:center)
petals = 3
for i in 1..petals do
  head.rotate(360/petals)
  head.oval(0,10,5,5,:center)
  head.oval(0,17,2,2,:center)
end
# randomize head attributes
head.randomize :fill, redcolors
head.randomize :stroke, bluecolors
head.randomize :scale, 0.2..2.0
head.randomize :rotation, 0..360

# create particles
total_particles = 100
total_iterations = 100
particles = []
for i in 0...total_particles do
  # start particle at random point at bottom of canvas
  x = random(canvas.width/2 - 50,canvas.width/2 + 50)
  p = Particle.new(x,0)
  p.velocity_x = random(-0.5,0.5)   # set initial x velocity
  p.velocity_y = random(1.0,3.0)    # set initial y velocity
  p.acceleration = 0.1            # set drag or acceleration
  particles[i] = p          # add particle to array
end

# animate particles
for frame in 0...total_iterations do
  for i in 0...total_particles do
    particles[i].move
  end
end

# draw particle trails and heads
for i in 0...total_particles do
  canvas.push
  # choose a stem color
  color = choose(bluecolors).a(0.7).analog(20,0.7)
  canvas.stroke(color)
  canvas.strokewidth(random(0.5,2.0))
  # draw the particle
  particles[i].draw(canvas)
  # go to the last particle position and draw the flower head
  canvas.translate(particles[i].points[-1][0],particles[i].points[-1][1])
  canvas.draw(head)
  canvas.pop
end

canvas.save
