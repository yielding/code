require 'gnuplot'

Gnuplot::SPlot.new do |plot|
    plot.title 'Spiral'
    plot.nokey
    plot.parametric
    plot.hidden3d
    plot.view '80,50'
    plot.isosamples '60,15'
    plot.xrange '[-8:8]'
    plot.yrange '[-8:8]'
    plot.zrange '[-8:8]'
    plot.urange '[-2*pi:2*pi]'
    plot.vrange '[-pi:pi]'
    plot.data << Gnuplot::DataSet.new('cos(u)*(cos(v)+3), sin(u)*(cos(v)+3), sin(v)+u') do |ds|
        ds.with = 'lines'
    end
end
