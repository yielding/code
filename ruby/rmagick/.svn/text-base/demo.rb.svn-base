#!/opt/local/bin/ruby -wKU

require 'RMagick'

canvas = Magick::Image.new(240, 300,
              Magick::HatchFill.new('white','lightcyan2'))
gc = Magick::Draw.new

# Draw curve
gc.stroke('blue')
gc.stroke_width(3)
gc.fill_opacity(0)
gc.bezier(45,150, 45,20, 195,280, 195,150)

# Draw endpoints
gc.stroke('gray50')
gc.stroke_width(1)
gc.circle(45,150, 49,150)
gc.circle(195,150, 199,150)

# Draw control points
gc.fill_opacity(1)
gc.fill('gray50')
gc.circle(45,17, 49,17)
gc.circle(195,280, 199,280)

# Connect the points
gc.line(45,150, 45,17)
gc.line(195,280, 195,150)

# Annotate
gc.stroke('transparent').fill('black')
gc.text(27, 175, "'45,150'")
gc.text(175,138, "'195,150'")
gc.text(55,22, "'45,20'")
gc.text(143,285, "'195,280'")
gc.draw(canvas)

canvas.write('bezier.gif')