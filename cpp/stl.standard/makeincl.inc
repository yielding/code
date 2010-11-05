#==========================================================================
#
#  makeincl.bcc - header file Borland C++ makefiles
#
#==========================================================================
#
#  (c) Copyright 1994, 1995 Rogue Wave Software, Inc.
#  ALL RIGHTS RESERVED
#
#  The software and information contained herein are proprietary to, and
#  comprise valuable trade secrets of, Rogue Wave Software, Inc., which
#  intends to preserve as trade secrets such software and information.
#  This software is furnished pursuant to a written license agreement and
#  may be used, copied, transmitted, and stored only in accordance with
#  the terms of such license and with the inclusion of the above copyright
#  notice.  This software and information or any other copies thereof may
#  not be provided or otherwise made available to any other person.
#
#  Notwithstanding any other lease or license that may pertain to, or
#  accompany the delivery of, this computer software and information, the
#  rights of the Government regarding its use, reproduction and disclosure
#  are as set forth in Section 52.227-19 of the FARS Computer
#  Software-Restricted Rights clause.
#
#  Use, duplication, or disclosure by the Government is subject to
#  restrictions as set forth in subparagraph (c)(1)(ii) of the Rights in
#  Technical Data and Computer Software clause at DFARS 252.227-7013.
#  Contractor/Manufacturer is Rogue Wave Software, Inc.,
#  P.O. Box 2328, Corvallis, Oregon 97339.
#
#  This computer software and information is distributed with "restricted
#  rights."  Use, duplication or disclosure is subject to restrictions as
#  set forth in NASA FAR SUP 18-52.227-79 (April 1985) "Commercial
#  Computer Software-Restricted Rights (April 1985)."  If the Clause at
#  18-52.227-74 "Rights in Data General" is specified in the contract,
#  then the "Alternate III" clause applies.
#
#==========================================================================
#
#  Header file for makefiles for the Rogue Wave Standard Library package,
#  using Borland C++ with a Win32 target.
#
#==========================================================================
#
#       Usage:
#
#
# make -fmakefile.bcc -DBINDING=<binding> -DTHREAD=<threads> \
#                -DBMODE=<build mode> -DENVIRON=<environ>
#
#
#      <environ> may be...          for...
#      ----------------------   ------------------------------------------
#     *WIN32                    32Bit Windows environment
#
#      <binding> may be...          for...
#      ----------------------   ------------------------------------------
#     *STATIC                 a statically linked version of the library
#      DLL                                  a dynamically linked version of the library
#
#
#      <thread> may be...           for...
#      ----------------------   ------------------------------------------
#     *SINGLE                       use with single-threaded applications
#      MULTI                             an "MT-safe" version of the library
#
#
#      <build mode> may be...   for...
#      ----------------------   ------------------------------------------
#      DEBUG                             a debug version of the library
#     *RELEASE                           a release version of the library
#
#
#==========================================================================
#
#  Examples:
#
#  (Assume building under Windows NT or 95):
#      make -fmakefile.bcc BINDING=DLL THREAD=MULTI
#       // builds or uses a flat-model dll version of the library, suitable for
#       //   use with multi-threaded applications, under Windows NT/95
#
#==========================================================================

###################################################################
#
#       Borland specific directives ---
#
.SWAP
.AUTODEPEND

###################################################################
#
#       set default values:

!ifndef ENVIRON
ENVIRON = WIN32
!endif

!ifndef BINDING
BINDING = STATIC
!endif

!ifndef THREAD
THREAD = MULTI
!endif

!ifndef BMODE
BMODE = RELEASE
!endif

###################################################################
#
# Flag illegal options:
#

!if $(ENVIRON) != WIN32
! error Illegal value for ENVIRON option
!endif

!if $(BINDING) != DLL && $(BINDING) != STATIC
!  error Illegal value for BINDING option
!endif

!if $(THREAD) != SINGLE && $(THREAD) != MULTI
!  error Illegal value for THREAD option
!endif

!if $(BMODE) != RELEASE && $(BMODE) != DEBUG
!  error Illegal value for BMODE option
!endif

###################################################################
#
# Set tool and version names:

!if $(ENVIRON) == WIN32
CPP        = bcc32
CPP32      = cpp32
LIBRARIAN  = tlib /P128
LINKER     = ilink32
RC         = brc32
ENVNAME    =
!endif

###################################################################
#
# Set the various flags:

!if $(BMODE) == DEBUG
DBGOPT= -v -N -x -xp
CCLINKOPT = -lGn
!else
CCLINKOPT = -lGn
!endif

!if $(THREAD) == MULTI
CCLINKOPT = $(CCLINKOPT) -tWM
LIBSUF=mt
!else
CCLINKOPT = $(CCLINKOPT) -tWM-
LIBSUF=
!endif

###################################################################
#
# Set any relevant defines (-Dxxx)

DEFOPTS =

!if $(BINDING) == DLL
DEFOPTS=$(DEFOPTS) -tWCR
TARGSUF=R
LIBSUF=$(LIBSUF)i
!else
DEFOPTS = $(DEFOPTS) -tWC
LIBSUF=$(LIBSUF)
TARGSUF=
!endif

###################################################################
#
# Set any compiler options

PCHROOT=stl_pch
CCOPTS = -w- -jb -j1 -Hc -H=$(PCHROOT).csm

#Compile flags:
CPPFLAGS= $(CCOPTS) $(DBGOPT)  $(ENVOPTS) $(DEFOPTS) $(THROPTS) $(CCLINKOPT)
LINKFLAGS= -Gn -Gi -Tpd -aa -L$(MAKEDIR)\..\lib -x
LINKSTARTUP= c0d32.obj
LINKLIBS=import32.lib cw32$(LIBSUF).lib
RCFLAGS= -r -i$(MAKEDIR)\..\include;$(MAKEDIR)\..\include\windows
