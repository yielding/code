1) port edit gcc46
  /congifure.args
  <remove ",fortran">

2) port install gcc46 comfigure.compiler=apple-gcc-4.2
3) port install gcc46 build.jobs=1
