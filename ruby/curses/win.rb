require 'curses'

Curses.init_screen()

my_str = "LOOK! PONIES!"
bwin = Curses::Window.new(10, (my_str.length + 10),
                          (Curses.lines - 10) / 2,
                          (Curses.cols - (my_str.length + 10)) / 2)
bwin.box("\\", "/")
bwin.refresh
win = bwin.subwin( 6, my_str.length + 6,
                  (Curses.lines - 6) / 2,
                  (Curses.cols - (my_str.length + 6)) / 2 )
win.setpos(2,3)
win.addstr(my_str)
# or even
win << "\nORLY"
win << "\nYES!! " + my_str
win.refresh
win.getch
win.close
