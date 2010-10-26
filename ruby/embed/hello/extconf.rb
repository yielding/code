require 'mkmf'

exit unless have_header('stdio.h')

dir_config('test')
create_makefile('test')
