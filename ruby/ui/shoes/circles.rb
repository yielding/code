Shoes.app(:width => 1000,  :height => 800) {
  fill        rgb(0, 0.6, 0.9, 0.1)
  stroke      rgb(0, 0.6, 0.9)
  strokewidth 0.25
  1000.times {
    oval :left   => (-5..self.width).rand,
         :top    => (-5..self.height).rand,
         :radius => (25..50).rand
  }
}