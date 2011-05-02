require "hotcocoa"

class Application
  include HotCocoa

  def start
    application :name => "Hello" do |app|
      app.delicate = self
      window :frame => [500, 500, 200, 100], :title => "Hello" do |win|
        win << label(:text => "Hello World", :layout => { :start => false })
        win.will_close { exit }
      end
    end
  end
end

Application.new.start
