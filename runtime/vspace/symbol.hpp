#include <boost/interprocess/allocators/allocator.hpp>
#include <boost/interprocess/containers/vector.hpp>
#include <boost/interprocess/containers/string.hpp>

namespace gecko {
  
  namespace vspace {
    
    namespace bip = boost::interprocess;
    
    // basic symbol
    
    template <typename A> struct symbol {

      enum status_t {NEW, USED, OLD, FREE}; // TODO mainly for GC
      
      // vectors of type T allocated in the segment
      typedef bip::allocator<void, A>          void_allocator_t;

      // shared strings
      typedef bip::basic_string<char, std::char_traits<char>, bip::allocator<char, A>> shared_string_t;
      
      // data
      shared_string_t name;
      status_t flags;

      // constructor
      symbol(const char* s, const void_allocator_t& void_alloc)
        : name(s, void_alloc), flags(NEW) {}
      

      // printer
      friend std::ostream& operator<<(std::ostream& os, const symbol& s) {
        os << "(" << s.name << ", " << s.flags << ")";
        return os;
      }
      
    };

    
    // elemental vectors
    
    template <typename T, std::size_t S, typename A>
    struct elemental_vector : public virtual symbol<A> {

      typedef bip::allocator<T, A>             bitv_allocator_t;
      typedef bip::vector<T, bitv_allocator_t> bitv_vector_t;
      typedef bip::allocator<bitv_vector_t, A> bitv_vector_allocator_t;

      typedef typename symbol<A>::void_allocator_t void_allocator_t;

      // data -- change implementation to smallest type
      bitv_vector_t elev;

      // constructor
      elemental_vector(const char* s, const void_allocator_t& void_alloc)
        : symbol<A>(s, void_alloc), elev(S, 0, void_alloc) {}
      

      // printer
      friend std::ostream& operator<<(std::ostream& os, const elemental_vector& s) {
        os << "(E: " << s.name << ", " << s.flags << ", " << s.elev.size() << ")";
        return os;
      }

    };


    // semantic vectors
    
    template <typename T, std::size_t N, std::size_t S, typename A>
    struct semantic_vector : public virtual symbol<A> {

      typedef bip::allocator<T, A>             bitv_allocator_t;
      typedef bip::vector<T, bitv_allocator_t> bitv_vector_t;
      typedef bip::allocator<bitv_vector_t, A> bitv_vector_allocator_t;

      typedef typename symbol<A>::void_allocator_t void_allocator_t;

      // data
      bitv_vector_t semv;


      // constructor
      semantic_vector(const char* s, const void_allocator_t& void_alloc)
        : symbol<A>(s, void_alloc), semv(N, 0, void_alloc) {}
      

      // printer
      friend std::ostream& operator<<(std::ostream& os, const semantic_vector& s) {
        os << "(S: " << s.name << ", " << s.flags << ", " << s.semv.size() << ")";
        return os;
      }
      
      // operations...
      
      void superpose(const elemental_vector<T,S,A>& other) {
        #pragma clang loop vectorize(enable) interleave(enable)
        for (std::size_t i = 0; i < N; ++i) {
          semv[i] |= other.elev[i];
        }
      }
      
      // ... under construction
    };

    
    // regular vectors inherit from both semantic and elemental behaviours

    template <typename T, std::size_t N, std::size_t S, typename A>
    struct vector : public elemental_vector<T,S,A>, public semantic_vector<T,N,S,A> {

      typedef bip::allocator<T, A>             bitv_allocator_t;
      typedef bip::vector<T, bitv_allocator_t> bitv_vector_t;
      typedef bip::allocator<bitv_vector_t, A> bitv_vector_allocator_t;

      typedef typename symbol<A>::void_allocator_t void_allocator_t;

      // constructor
      vector(const char* s, const void_allocator_t& void_alloc) :
        symbol<A>(s, void_alloc),
        semantic_vector<T,N,S,A>(s, void_alloc),
        elemental_vector<T,S,A>(s, void_alloc) {}
      

      // printer
      friend std::ostream& operator<<(std::ostream& os, const vector& s) {
        os << "(C: " << s.name << ", " << s.flags << ", " << s.semv.size() << ", " << s.elev.size() << ")";
        return os;
      }
      
    };

  }
}
