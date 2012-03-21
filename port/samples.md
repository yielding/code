configuration
=============
  1. port edit gcc46
    /congifure.args
    <remove ",fortran">

  2. port install gcc46 configure.compiler=apple-gcc-4.2
  3. port install gcc46 build.jobs=1


uninstall
==========
  1. sudo port -fp uninstall --follow-dependents installed
  2. sudo port -fp unisntall xxx
