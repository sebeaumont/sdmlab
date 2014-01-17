// -*- compile-command: "clang++ -S -std=c++11 -emit-llvm binvec.cpp -o binvec++.ll"

#include "binvec.hpp"

namespace dsm {
  typedef binary_vector<unsigned int, 1024> vector;
  
  vector* make_vector() {
    return new vector();
  }
}
