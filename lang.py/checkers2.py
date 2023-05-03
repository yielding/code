from Tkinter import *
import random, string, time

class CheckersInterface:
    DEBUG=1
    DEBUG_BIG_THINGS=0
    DEBUG_PRINT_FUNCTIONS=0
    #The Tuneable Constants
    DELAY=0 #885=1 sec.
    SQUARESIZE=50
    PIECE_DIAMETER=35
    
    def __init__(self, master=None):
        """This is the constructor. It includes some basic startup work that
        would not fit anywhere else, and then it calls self.begin_new_game.  It does
        not require any variables."""

        
        self.piece_offset=(self.SQUARESIZE-self.PIECE_DIAMETER)#calulation saver
        
        if master==None: #master creator
           master=Tk()
        self.master=master
        self.master.title("Checkers")
        if self.DEBUG:
            self.master.bind("<2>", self.remove_piece)
        self.master.protocol("WM_DELETE_WINDOW", self.end) #handle the exit
        self.master.bind("<Escape>", self.end)
        #self.master.bind("t", self.erace_temporary)
                #/|\ 
                # |  there are no temporary objects
        self.master.bind("n", self.show_numbers_toggle)
        self.master.bind("[", self.go_to_move)
        self.master.bind("a", self.toggle_add_mode)
        self.master.bind("r", self.toggle_remove_mode)
        self.make_display()
        
        self.begin_new_game()
        
    def toggle_add_mode(self, event):
        """This will put the game in add mode.  In this mode, you can add red
        pieces with a left click and black pieces with a right click."""
        if self.add_mode:
           self.add_mode=0
           self.master.unbind("<1>")
           self.master.unbind("<2>")
           
           self.c.tag_bind("pieces", "<1>", self.get_piece_click)
           self.c.tag_bind("squares", "<1>", self.get_square_click)
        else:
            self.add_mode=1
            self.master.bind("<1>", self.make_a_piece)
            self.master.bind("<2>", self.make_a_piece)

            self.remove_mode=0
            self.c.tag_unbind("pieces", "<1>")
            self.c.tag_unbind("squares", "<1>")

    def toggle_remove_mode(self, event):
        if self.remove_mode:
            self.remove_mode=0
            self.master.unbind("<1>")
            
            self.c.tag_bind("pieces", "<1>", self.get_piece_click)
            self.c.tag_bind("squares", "<1>", self.get_square_click)
        else:
            self.remove_mode=1
            self.master.bind("<1>", self.remove_piece)

            self.add_mode=0
            self.master.unbind("<2>")
            self.c.tag_unbind("pieces", "<1>")
            self.c.tag_unbind("squares", "<1>")

    def make_display(self):
        """This function will create the Canvas for the board, and then the board.
        The variables required by this function are:
            self.master, self.SQUARESIZE."""
        foo=self.SQUARESIZE*8 #calculation saver
        self.c=Canvas(self.master, height=foo, width=foo)
        self.message=Label(self.master, text="", bd=2, relief=RAISED, font=("Dungeon", "10", ""))
        self.make_checker_squares(0,7,"bisque")
        self.make_checker_squares(1,8,"green3", "squares")

        
        history_scroll=Scrollbar(self.master)
        self.history_display=Listbox(self.master, yscrollcommand=history_scroll.set)
        history_scroll.config(command=self.history_display.yview)
        self.history_display.bind("<Double-Button-1>", self.go_to_move)
        self.c.grid(row=1, column=0)
        self.message.grid(row=0, column=0, columnspan=3, pady=5)
        self.history_display.grid(row=1, column=1, sticky=N+S)
        history_scroll.grid(row=1, column=2, sticky=N+S)
        
        for baz in self.c.find_overlapping(self.SQUARESIZE*1.5, self.SQUARESIZE*0.5,\
                                          self.SQUARESIZE*1.5, self.SQUARESIZE*0.5):
            if self.c.type(baz)=="rectangle":
                self.upper_corner_square=baz

    def show_numbers_toggle(self, event=None):
        """This shows little white numbers in the corner of each square"""
        if self.c.type("numbers") == "text":
            self.c.delete("numbers")
        else:
            number=1
            start=self.SQUARESIZE+10; stop=self.SQUARESIZE*8
            for y in range(10, self.SQUARESIZE*8, self.SQUARESIZE):
                for x in range(start,stop, self.SQUARESIZE*2):
                    self.c.create_text(x, y, text=str(number), fill="white", tags="numbers")
                    number = number + 1
                if start==self.SQUARESIZE+10:
                    start=10; stop=self.SQUARESIZE*7
                else:
                    if start==10:
                        start=self.SQUARESIZE+10; stop=self.SQUARESIZE*8
            
    def begin_new_game(self):
        """This is the function that begins a new game.  It will be run whenever
        a new game is needed.  It clears various variables, creates the pieces
        using make_pieces, binds the pieces and squares, binds the exit, and
        sets self.moving to the player who starts.  It then calls self.MoveLoop.
            This function requires self.message, and module random"""
        if self.DELAY:
            self.message.config(text="Creating new game...", fg="purple")
        
        #variable clearing
        self.c.itemconfig("squares", width=1, outline="black")
        self.quux=None #temporary storage
        self.pieces= {"black":[], "red":[]} #first list is black's pieces, then red's pieces.
        self.piece=None
        self.piece_square=None
        self.square=()
        self.count=-1
        self.oldmessage_info=["", ""]
        self.c.delete("pieces")
        self.jumps = [[],[]]
        self.jump_made=None
        self.c.delete("win_text")
        self.history=[]
        self.history_display.delete(0, END)

        #flag setting
        self.got_move=0
        self.got_piece=0
        self.end_now=0
        self.add_mode=0
        self.remove_mode=0
        
        self.make_pieces("black", self.DELAY)
        self.make_pieces("red", self.DELAY)
        
        self.c.tag_bind("pieces", "<1>", self.get_piece_click)
        self.c.tag_bind("squares", "<1>", self.get_square_click)

        self.moving= "black" #reversed since setup_move will switch it.
        
        if self.DEBUG_BIG_THINGS:
            print self.pieces

        
        self.MoveLoop()
 
    def MoveLoop(self):
        """This is the central function. It's main portion is a loop.  The loop is
        terminated by self.GameDone returning true.  Within the loop it waits for
        a move to be gotten, then calls self.check_move, and self.do_move.  Once
        the loop terminates, MoveLoop calls self.AnotherGame and deals with the
        response."""
        self.setup_move()
        while not self.GameDone():
            if self.end_now:
                break
            self.master.update()
            if self.got_move:
                if not self.check_move():
                    #whenever a move is gotten which is correct, do this stuff
                    self.do_move()
                    self.cleanup_move(2)
                    self.setup_move()
                    self.cleanup_move(3)
                #whenever a move is goten, do this stuff
                self.cleanup_move(1)
        if self.GameDone() == 2:
            self.c.create_text(int(self.c.cget("height"))/2, \
                               int(self.c.cget("width"))/2,\
                               text="Black Won!!!", fill="black",\
                               font=("Dungeon", "20", ""), tag="win_text")
        if self.GameDone() == 1:
            self.c.create_text(int(self.c.cget("height"))/2,\
                               int(self.c.cget("width"))/2,\
                               text="Red Won!!!", fill="black",\
                               font=("Dungeon", "23", ""), tag="win_text")
            self.c.create_text(int(self.c.cget("height"))/2,\
                               int(self.c.cget("width"))/2,\
                               text="Red Won!!!", fill="red",\
                               font=("Dungeon", "20", ""), tag="win_text")
            start=time.time()
            #while start-time.time() < 3:
            #    self.master.update()

        if self.AnotherGame():
            self.begin_new_game()
        else:
            self.master.destroy()
            
#++++++++++++++++++++++++++++++++++++++++more detailed functions+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    def cleanup_move(self, type):
        """This function clears various variables.  It has a type argument so
        that it can clean only partway so other functions can use some of the
        variables"""
        if type ==3:
            self.jump_made=None
            self.piece=None
        if type ==2:
            self.got_piece=0
            self.c.itemconfig(self.piece_square,outline="black", width=1)
            self.jumps=[[],[]]
        if type ==1:
            self.got_move=0
            self.square=()

    def setup_move(self):
        """This does the setup required of a move.  It checks for kings to be
        crowned, and checks for double jumps with the check_for_jumps function.
        If there are no double jumps, it toggles self.moving, sets the
        history, and checks for jumps again."""
        if self.DEBUG_PRINT_FUNCTIONS:
            print "setup_move"
        if self.DEBUG:
            print "lengh of history:", len(self.history)   
            print "count:", self.count

        #kingmaker part
        not_crowning=1
        for piece in self.pieces[self.moving]:
            if self.DEBUG_BIG_THINGS:
                print self.c.coords(piece)
            if self.moving == "black":
                if self.c.coords(piece)[1]== self.piece_offset:
                    if self.DEBUG:
                        print "black kings!"
                    not_crowning=0
                    self.c.itemconfig(piece, outline="gold2", width=3)
            if self.moving == "red":
                if self.c.coords(piece)[1] == (self.SQUARESIZE*7)+self.piece_offset:
                    if self.DEBUG:
                        print "red kings!"
                    not_crowning=0
                    self.c.itemconfig(piece, outline="gold2", width=3)

        #Dobble Jump checker
        self.check_for_jumps()
        if self.DEBUG_BIG_THINGS:
            print self.jumps[0], self.jump_made
        same_piece=0
        for foo in self.jumps[0]:
            if foo[0] == self.piece:
                same_piece= 1
        if same_piece and not_crowning and self.jump_made != None:
            self.show_message("DOUBLE JUMP!!", 1)
            return 0
        else:
            #this creates a new turn.
            self.count=self.count+1
            self.history.append([])
            for foo in self.pieces["red"]+self.pieces["black"]:
                self.history[-1].append((self.c.itemcget(foo, "fill"), self.c.coords(foo),\
                                         self.c.itemcget(foo, "width")))
            if self.DEBUG_BIG_THINGS:
                print self.history
            self.jumps=[[], []]
            if self.moving=="black":
                self.moving="red"
                self.message.config(text="Red's move!", fg="red")
            else:
                self.moving="black"
                self.message.config(text="Black's move!", fg="black")
            if self.DEBUG:
                print "changed"
            self.check_for_jumps()

        if self.DEBUG:
            print "lengh of history:", len(self.history)
            
    def get_piece_click(self, event):
        """This function is called when a piece is clicked on.  It sets
        self.got_piece, and assigns the id of the piece clicked on to
        self.piece"""
        if self.DEBUG_PRINT_FUNCTIONS:
            pass; print "got_piece_click"
        if self.piece != None:
            self.c.itemconfig(self.piece_square, outline="black", width=1)
        try:
            self.piece_square, self.piece=self.c.find_overlapping(event.x, event.y, event.x, event.y)
        except ValueError:
            return 0
        self.got_piece=1
        
        if self.check_piece(): #positive numbers are failure, for check_piece
            self.piece_square=None
            self.piece=None
            self.got_piece=0
        else:
            self.c.itemconfig(self.piece_square, outline="blue", width=3)

    def check_piece(self):
        """check_piece is called after get_piece returns.  It checks the color
        of the piece and currently does nothing else."""
        if self.DEBUG_PRINT_FUNCTIONS:
            pass; print "check_piece"
        #correct player checker
        if self.c.itemcget(self.piece, "fill") != self.moving:
            self.show_message("That is not your piece!")

            return 1
        return 0
        
    def get_square_click(self, event):
        """This function is called when a square is clicked on.  It only acts if self.got_piece has been
        set before.  When it acts, it sets self.got_move, and assigns the id of the square clicked on to
        self.square."""
        if self.DEBUG_PRINT_FUNCTIONS:
            pass; print "got_square_click"
        if self.got_piece:
            self.square=self.c.find_overlapping(event.x, event.y, event.x, event.y)
            if self.DEBUG:
                print "got square:", self.square
            self.got_move=1
   
    def check_move(self):
        """This function does all the verifiying required for a move.  It checks
        for errors in the get_piece and get_square functions that cause no move
        to be reported. It then calulates some variables used in later checks.
        Then it checks if the move is a jump(if there are any jumps).  It then
        checks the direction of the move, the move's distence, and finally it
        includes a check for a piece in the square to be moved to, if this has
        not been caught before."""
        
        if self.DEBUG_PRINT_FUNCTIONS:
            pass; print "check_move"

        if len(self.square) != 1 or self.piece == None:
            if self.DEBUG:
                print "missing piece or square!"
            return 5
        sqr_cords=self.c.coords(self.square) #square coords
        sqr_cntr=apply(self.find_center, sqr_cords) #square center
        pce_cntr=apply(self.find_center, self.c.coords(self.piece)) #piece center
        vtr=(sqr_cntr[0]-pce_cntr[0], sqr_cntr[1]-pce_cntr[1])#piece vector(distence and direction)
        if self.DEBUG:
            pass;#print sqr_cords, sqr_cntr, pce_cntr, vtr

        if self.jumps[0]: #jump checker
            #if move has not been found by check_for_jumps then fail
            #else, ingore all the other checks, and succeed
            if self.jumps[0].count((self.piece, vtr)) != 1:
                self.show_message("You have a jump!", .8)
                return 5
            else:
                self.jump_made=self.jumps[0].index((self.piece, vtr))
                if self.DEBUG:
                    print "jump_made: ", self.jump_made
                return 0
            
        #movement direction checker
        if self.c.itemcget(self.piece, "outline") != "gold2":
            if self.moving== "black":
                if vtr[1] > 0:
                    if self.DEBUG:
                        print "wrong way, black!"
                    return 3
            else:
                if vtr[1] < 0:
                    if self.DEBUG:
                        print "wrong way, red!"                
                    return 3

        #distence checker        
        if abs(vtr[0]) != self.SQUARESIZE or abs(vtr[1]) != self.SQUARESIZE:
            if self.DEBUG:
                print "Too far!"            
            return 4
        
        #square emptiness checker
        if self.c.type(self.c.find_overlapping(sqr_cords[0]+(self.SQUARESIZE/2), \
                                               sqr_cords[1]+(self.SQUARESIZE/2), \
                                               sqr_cords[2]-(self.SQUARESIZE/2), \
                                               sqr_cords[3]-(self.SQUARESIZE/2))) != "rectangle":
           if self.DEBUG:
               print "not empty: ",self.c.find_overlapping(sqr_cords[0]+(self.SQUARESIZE/2), \
                                               sqr_cords[1]+(self.SQUARESIZE/2), \
                                               sqr_cords[2]-(self.SQUARESIZE/2), \
                                               sqr_cords[3]-(self.SQUARESIZE/2))
           return 2
       
        return 0
      
    def do_move(self):
        """This function actually moves the piece in self.piece to the square
        in self.square.  It also handdles various jumping details."""
        if self.DEBUG_PRINT_FUNCTIONS:
            pass; print "do_move"
        self.history_display.insert("end", str((self.piece_square-self.upper_corner_square)+1)+"-"+
                            str((self.square[0]-self.upper_corner_square)+1))
        
            
        if self.jumps[0]:
            foo=self.pieces.keys() #ugly hack to get the other color's pieces
            foo.remove(self.moving)
            
            self.pieces[foo[0]].remove(self.jumps[1][self.jump_made])
            self.c.delete(self.jumps[1][self.jump_made])
  
        foo=self.c.coords(self.square) #calulation saver
        self.c.coords(self.piece,\
                      foo[0]+self.piece_offset, foo[1]+self.piece_offset,\
                      foo[2]-self.piece_offset, foo[3]-self.piece_offset)

            
    
    def GameDone(self):
        """This is the win checker.  It reports 0 if the game has not ended,
        2 for a win by red, 1 for a win by black, and 3 for a draw."""
        if self.DEBUG_PRINT_FUNCTIONS:
            pass; #print "GameDone"
        if self.pieces["black"] == []:
            return 1
        if self.pieces["red"] == []:
            return 2
        return 0

    def AnotherGame(self):
        
        """This function asks if another game is wanted, and reports true or false,
        depending on the answer.  It requires module string,
        self.message, and self.master"""
        if self.DEBUG_PRINT_FUNCTIONS:
            pass; #print "AnotherGame"
        self.c.create_rectangle(0,0, int(self.c.cget('width')), int(self.c.cget('height')), \
                                stipple='gray50', fill='black', tag='end_game_overlay')
        self.message.config(text="Do you want another game?", fg="gray25")
        global ans
        def answer(event):
            global ans
            ans=event.char
        self.master.bind("y", answer); self.master.bind("Y", answer)
        self.master.bind("n", answer); self.master.bind("N", answer)
        ans=""
        while ans == "":
            self.master.update()
        self.master.unbind("y"); self.master.unbind("Y")
        self.master.unbind("n"); self.master.unbind("N")
        self.c.delete('end_game_overlay')
        if string.lower(ans) == "y":
            return 1
        else:
            return 0
    
#++++++++++++++++++++++++++++++++++++++++++++++helper functions++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    def go_to_move(self, event=None, move_number=None,):
        """This function will recreate previous positions by recreating all the pieces from the information
        in self.history"""
        if move_number == None:
            move_number = self.count-1
        if event.widget==self.history_display:
            move_number=self.history_display.index("@"+str(event.x)+","+str(event.y))
        if self.DEBUG:
            print "move_number:", move_number
        if move_number < 0:
            return 1
        
        self.c.delete("pieces")
        self.pieces["black"]= []
        self.pieces["red"] = []
        for foo in self.history[move_number]:
            self.pieces[foo[0]].append(apply(self.c.create_oval, foo[1], {"fill":foo[0], "tag":"pieces"}))
            if foo[2] == 3:
                self.c.itemconfig(self.pieces[foo[0]][-1], width=3, outline="gold")
        if move_number%2 == 0: #not reversed since setup_move will flip them _twice_.
            self.moving="black"
        else:
            self.moving="red"
        self.cleanup_move(1)
        self.cleanup_move(2)
        self.setup_move()
        self.cleanup_move(3)
        self.count=move_number
        self.history=self.history[:move_number+1]
        self.history_display.delete(move_number, END)
        if self.DEBUG:
            print "lengh of history:", len(self.history)
        
    def check_for_jumps(self):
        """This function checks all the possible jumps for self.moving pieces"""
        pass
        if self.DEBUG_PRINT_FUNCTIONS:
            print "check_for_jumps"
        if self.moving=="red":
            baz_normal=[(2*self.SQUARESIZE, 2*self.SQUARESIZE), (-2*self.SQUARESIZE, 2*self.SQUARESIZE)]
        if self.moving=="black":
            baz_normal=[(2*self.SQUARESIZE, -2*self.SQUARESIZE), (-2*self.SQUARESIZE, -2*self.SQUARESIZE)]
        baz=baz_normal
        for piece in self.pieces[self.moving]:
            if self.c.itemcget(piece, "outline") == "gold2":
                baz=[(2*self.SQUARESIZE, 2*self.SQUARESIZE),\
                     (-2*self.SQUARESIZE, 2*self.SQUARESIZE),
                     (2*self.SQUARESIZE, -2*self.SQUARESIZE),\
                     (-2*self.SQUARESIZE, -2*self.SQUARESIZE)]
            else:
                baz=baz_normal
            for vtr in baz:
                bar=self.c.coords(piece)
                sqr_cords=(bar[0]-self.piece_offset+vtr[0],\
                           bar[1]-self.piece_offset+vtr[1],\
                           bar[2]+self.piece_offset+vtr[0],\
                           bar[3]+self.piece_offset+vtr[1])
                if self.jumpable(vtr, sqr_cords):
                    if len(self.c.find_overlapping(sqr_cords[0]+5, sqr_cords[1]+5, \
                                                   sqr_cords[2]-5, sqr_cords[3]-5))==1:
                        self.jumps[0].append(piece, vtr)
                        self.jumps[1].append(self.quux)
                self.quux=None
        if self.DEBUG_BIG_THINGS:
            print self.jumps

    def jumpable(self, vtr, sqr_coords):
        """This function will determine, based on self.piece & self.square,
        if a move is a legal jump."""
        
        if self.DEBUG_PRINT_FUNCTIONS:
            pass#; print "jumpable"

        if abs(vtr[0]) != self.SQUARESIZE*2 or abs(vtr[1]) != self.SQUARESIZE*2:
            return 0 #if the move is not two squares, diagonaly, then fail

        barX=-self.SQUARESIZE*(vtr[0]/abs(vtr[0])) #the X direction of the jump
        barY=-self.SQUARESIZE*(vtr[1]/abs(vtr[1])) #the Y direction of the jump
        try:
            self.quux=self.c.find_enclosed(sqr_coords[0]+barX, sqr_coords[1]+barY, \
                                     sqr_coords[2]+barX, sqr_coords[3]+barY)[0]
        except IndexError:
            return 0 #if there is no piece to be jumped, somehow, then fail
        
        #This is a debuging test that generates too much data, so I comented it out.
##        if self.DEBUG:
##            print sqr_coords[0]+barX, sqr_coords[1]+barY, \
##                                 sqr_coords[2]+barX, sqr_coords[3]+barY
##            self.c.create_rectangle(sqr_coords[0]+barX, sqr_coords[1]+barY, \
##                                 sqr_coords[2]+barX, sqr_coords[3]+barY, width=3, outline="purple", tag="temporary")

        foo=self.pieces.keys() #ugly hack to get the other color's pieces
        foo.remove(self.moving)
        
        if self.pieces[foo[0]].count(self.quux) == 1:
            if self.DEBUG: #if the piece to be jumped is the opponents piece,
                print "yes!" # then succeed
            return 1
        return 0

    def end(self, event=None):
        """This function simply sets the self.end_now variable so the loop in MoveLoop will break."""
        self.end_now=1
        
    def make_checker_squares(self, start, stop, color, tags=""):
        """This function will create a checkerboard of squares, of the color given, with the tags given.
        The start and stop arguments are a technical way of specifiying which half of the checkerboard is
        to be created.
            The variables required by this function are:
                self.c(a Canvas), self.SQUARESIZE, """
##        if self.DEBUG_BIG_THINGS:
##            print color
        for y in range(0,8):
            for x in range(start, stop, 2):
                
                baz=self.c.create_rectangle(x*self.SQUARESIZE,\
                                            y*self.SQUARESIZE,\
                                            (x+1)*self.SQUARESIZE, \
                                            (y+1)*self.SQUARESIZE, \
                                            fill=color, tag=tags)
##                if self.DEBUG_BIG_THINGS:
##                    print baz,
##            if self.DEBUG_BIG_THINGS:
##                print
            if start==0:
                start=1; stop=8
            else:
                if start==1:
                    start=0; stop=7
                else:
                    raise Exception, "Incorrect value for start in make_checker_squares"

    def make_pieces(self, color, delay):
        """This function will make, and place in standard starting position, all the pieces for a specified
        color.  The color can be either "black" or "red".  If it is 0, they are placed on the top half of the board, if it is 1, on the bottom.
            The pieces are appended to the list variable corosponding to the color given, and they are given
            the tag "pieces".  The delay argument sets a delay(duh!), the unit
            is about 885 per sec.            
            The variables required by this function are:
                self.pieces(a dictionary of two lists, one for each side), self.c(a Canvas),
                self.SQUARESIZE, self.piece_offset"""
        
        side=self.pieces[color]
        if color=="red":
            start=1; stop=8
            start2=0; stop2=3
        else:
            start=0; stop=7
            start2=5; stop2=8
        for y in range(start2, stop2):
            for x in range(start, stop, 2):
                for delay_counters in range(delay):
                    self.master.update()
                side.append(self.c.create_oval(x*self.SQUARESIZE+self.piece_offset,\
                                               y*self.SQUARESIZE+self.piece_offset,\
                                               (x+1)*self.SQUARESIZE-self.piece_offset,\
                                               (y+1)*self.SQUARESIZE-self.piece_offset,\
                                               fill=color, tag="pieces"))
            
            if start==0:
                start=1; stop=8
            else:
                if start==1:
                    start=0; stop=7
                else:
                    raise Exception, "Incorrect value for start in make_pieces"

    def make_a_piece(self, event):
        if event.num==1:
            color='black'
        else:
            color='red'
        if len(self.c.find_overlapping(event.x, event.y, event.x, event.y)) ==1:
            cords=self.c.coords(self.c.find_overlapping(event.x, event.y, event.x, event.y))
            self.pieces[color].append(self.c.create_oval(cords[0]+self.piece_offset,\
                                                           cords[1]+self.piece_offset,\
                                                           cords[2]-self.piece_offset,\
                                                           cords[3]-self.piece_offset,\
                                                           fill=color, tag='pieces'))
            
    def find_center(self, x0, y0, x1, y1):
        """This will find the center of a box given by x0, y0 and x1, y1."""
        if self.DEBUG_PRINT_FUNCTIONS:
            pass; print "find_center"
        return ((x1-x0)/2+x0,(y1-y0)/2+y0)
    
    def show_message(self, message, seconds=0.8, color="gray25"):
        """This function sets the message widget to the value of message and
        the fg to color.  It's main use is to show a message for a time, then
        replace it with the previous message"""
        self.oldmessage_info[0]=self.message.cget("text")
        self.oldmessage_info[1]=self.message.cget("foreground")
        self.message.config(text=message, fg=color)
        self.master.after(int(seconds*1000), self.restore_message)
    
    def restore_message(self, event=None):
        """This function sets the message widget to the value of
        self.oldmessage_info[0] and the fg to self.oldmessage_info[1].
        It's main use is to show a message for a time, then replace it with the
        previous message"""
        if self.DEBUG_PRINT_FUNCTIONS:
            pass; print "restore_message"
        self.message.config(text=self.oldmessage_info[0])
        self.message.config(fg=self.oldmessage_info[1])

    def erace_temporary(self, evnet=None):
        """THis removes any objects on the canvas with the temporary tag"""
        if self.DEBUG_PRINT_FUNCTIONS:
            print "erace_temporary"
        self.c.delete("temporary")

    def remove_piece(self, event=None):
        """This is a function which will remove the piece which is
        clicked on."""
        piece=self.c.find_overlapping(event.x, event.y, event.x, event.y)
        print piece
        if len(piece) == 2 and self.c.type(piece[1]) == "oval":
            piece=piece[1]
            self.c.delete(piece)
            try:
                self.pieces["red"].remove(piece)
            except:
                self.pieces["black"].remove(piece)
        else:
            if self.DEBUG:
                print "Not a piece!"
        
CI=CheckersInterface()
