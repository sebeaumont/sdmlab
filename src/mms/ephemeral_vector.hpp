// Copyright (c) 2017 Simon Beaumont - All Rights Reserved.

#pragma once

namespace molemind {
  
  namespace sdm {

    namespace mms {

      
      template <typename T, std::size_t L, typename I>
      struct ephemeral_vector {
        
        ephemeral_vector() {
        }
        
        ephemeral_vector(const I& v) {
          for (std::size_t i = 0; i < L; i++) {
            _elem[i] = v[i];
          }
        }
        
      private:
        T _elem[L];
      };
    }
  }
}
