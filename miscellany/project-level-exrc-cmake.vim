" CMake
let &makeprg = "cd build/debug && make -j 16"
" note 16 is hardware concurrency for build
" make does an "out of tree build," thats why we cd into a diff directory
" this assumes you've run cmake ../.. inside build/debug ahead of time to generate the makefile
