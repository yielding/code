#!/bin/sh
env ac_cv_func_getpgrp_void=no \
    ac_cv_func_setpgrp_void=yes \
    rb_cv_negative_time_t=no \
    ac_cv_func_memcmp_working=yes \
    rb_cv_binary_elf=no \
    CPPFLAGS=/Users/yielding/ruby-mingw32/lib/ruby/1.8/i386-mingw32 \
    ./configure \
    --host=i386-mingw32 \
    --target=i386-mingw32 \
    --build=i686-darwin9 \
    --includedir=/Users/yielding/ruby-mingw32/lib/ruby/1.8/i386-mingw32 \
    --prefix=/usr/local/i386-mingw32-3.4.5/i386-mingw32
