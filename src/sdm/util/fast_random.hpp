//
//  fast_random.h
//
//  Copyright (c) 2012 Simon Beaumont All rights reserved.
//

#ifndef dsm_fast_random_h
#define dsm_fast_random_h

#include <boost/random/mersenne_twister.hpp>
#include <boost/random/discrete_distribution.hpp>
//#include <boost/random/random_number_generator.hpp>
#include <vector>
#include <algorithm>


namespace dsm { namespace random {

    // adapter encapsulates RNG
 
    struct adapter : std::unary_function<unsigned, unsigned> {
     
      boost::random::mt19937& _rng;
     
      unsigned operator()(unsigned i) {
        boost::random::uniform_int_distribution<> dist(0, i - 1);
        return dist(_rng);
      }

      // c'tor
      adapter(boost::random::mt19937& g) : _rng(g) {}
      
    };



   class random_cmrrsr {
    
    private:
      uint32_t x, y, z;
      
    public:
    
      //random_cmrrsr(random_cmrrsr const& r) : x(r.x), y(r.y), z(r.z) {} const
      
      random_cmrrsr(const uint32_t s) {
        seed(s);
      }
      

      inline void seed(uint32_t k) {

        x = (k >> 16)    + 4125832013u; // upper 16 bits + offset
        y = (k & 0xffff) +  814584116u; // lower 16 bits + offset
        z = 542;
      }
    
      
      inline uint32_t rand(void) {
        
        x *=  255519323u; x = ROTL(x,13); // CMR, period = 4294785923 (prime)
        y *= 3166389663u; y = ROTL(y,17); // CMR, period = 4294315741 (prime)
        z -= ROTL(z,11);  z = ROTL(z,27); // RSR, period = 253691 = 2^3*3^2*71*557
      
        return x ^ y ^ z;
      }
      
      
      inline uint32_t uniform_distribution(std::size_t b) {
        uint32_t r = rand();
        return (r / (float) UINT32_MAX) * b;
      }
      
    };


    struct my_adapter : std::unary_function<unsigned, unsigned> {
     
      random_cmrrsr& _rng;
     
      unsigned operator()(unsigned i) {
        return _rng.uniform_distribution(i) % i;
      }
     
      my_adapter(random_cmrrsr& g) : _rng(g) {}
      
    };
   
    

    // shuffle based vector randomization tools
   
    class vrand {
      
    private:
      
      //boost::random::mt19937 _mt = boost::random::mt19937();
      //adapter generator = adapter(_mt);
      random_cmrrsr _cmrrsr; //= random_cmrrsr(428394849);
      my_adapter _generator; //= my_adapter(_cmrrsr);
      std::vector<std::size_t> _idx;

    public:
     
      vrand(std::size_t seed, std::size_t n) : _cmrrsr(random_cmrrsr(seed)),
                                               _generator(my_adapter(_cmrrsr)) {

       // initialize list of indexes one time
       for (std::size_t i = 0; i < n; ++i)
         _idx.push_back(i);
      }
      
     
      // generate a shuffled vector of indexes could fix/template length of this

      inline std::vector<std::size_t> indexes() {
      
       std::random_shuffle(_idx.begin(), _idx.end(), _generator);
       return _idx;
      }

      inline uint32_t uniform_distribution(std::size_t b) {
        return _cmrrsr.uniform_distribution(b);
      }
 
    };
    
  }
}

#endif
