=begin
How do you find what methods or attributes to call/use?, there are
several ways to find them:
+ one is by using Word itself: go to Tools > Macro > Visual Basic
Editor and once there go to View > Object Browser, then go crazy and
experiment with the gazillion options.
+ Open a doc through IRB (see sample code below), and inspect objects
by something like word_object.ole_methods (sadly you can't .sort them)
+ download and run a Ruby OLE browser, again go crazy. One can be
found at http://dave.burt.id.au/ruby/ole_browser.rb

Now some Ruby:
=end

require 'win32ole'
word = WIN32OLE.new('word.application')
word.visible = true
word.documents.count

# open/create new document
word.documents.add

# or open file
word.documents.open(path_to_file)

# type something
word.selection.typetext("Hello World!\n")

# select whole text
word.selection.wholestory

# delete selection
word.selection.delete

# move to start of document
word.selection.start = 0
word.selection.end = 0

# search for a string
word.selection.find.text = 'my search'
result = word.selection.find.execute

# read the selection
puts word.selection.text
# and position
puts word.selection.start
puts word.selection.end
# or set the position, and selection
word.selection.start = 20
word.selection.end = 23
puts word.selection.text

# printing
word.options.printbackground = false
word.activedocument.PrintOut

# SAVING document
word.activedocument.saveas file_name, wdFormatText
# notice the 2nd parameter which is a numeric constant
# indicating the file format to save in thusly:
# I believe omitting 2nd parameter would save in native .doc)
#   wdFormatDocument = 0 (no conversion)
#   wdFormatTemplate = 1
#   wdFormatText = 2
#   wdFormatTextLineBreaks = 3
#   wdFormatDOSText = 4
#   wdFormatDOSTextLineBreaks = 5
#   wdFormatRTF = 6
#   wdFormatUnicodeText = 7  # it repeats!
#   wdFormatEncodedText = 7
#   wdFormatHTML = 8
#   wdFormatWebArchive = 9
#   wdFormatFilteredHTML = 10
#   wdFormatXML = 11


# close document
word.activedocument.close( true )  # presents save dialog box
word.activedocument.close( false ) # no save dialog, just close it

# quit word
word.quit
