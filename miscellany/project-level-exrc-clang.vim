" clang
set makeprg=clang++\ \-Wall\ -Werror\ -Wpedantic\ \-fstandalone-debug\ -std=c++17\ -D_GLIBCXX_DEBUG\ -g\ -o\ build/%<\ %

