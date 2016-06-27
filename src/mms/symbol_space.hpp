/* Copyright (C) Simon Beaumont 2015-2016 - All Rights Reserved */
#pragma once

#include <boost/interprocess/containers/string.hpp>
#include <boost/interprocess/containers/vector.hpp>
#include <boost/multi_index_container.hpp>
#include <boost/multi_index/ordered_index.hpp>
#include <boost/multi_index/hashed_index.hpp>
#include <boost/multi_index/random_access_index.hpp>
#include <boost/multi_index/member.hpp>
#include <boost/optional.hpp>

#include "elemental_vector.hpp"


#ifdef HAVE_DISPATCH
#include <dispatch/dispatch.h>
#else
// openMP
#endif

/////////////////////
// TODO make config

#define VELEMENT_64 1

#ifdef VELEMENT_64
#define ONE 1ULL
#else
#define ONE 1U
#endif
#define CHAR_BITS (8)

/////////////////////

namespace molemind { namespace sdm {
  
  namespace mms {
    
    using namespace molemind;
    using boost::multi_index_container;
    using namespace boost::multi_index;
    namespace bip = boost::interprocess;

    /* 
     * this class template can be instantiated in runtime library
     * source or inlined in application code; the important
     * implementation details are the types and sizes of the vectors
     * of the underlying vector space and the sparsity of the random
     * (a.k.a. elemental) vectors
     */

    template <typename VectorElementType, std::size_t VArraySize, std::size_t ElementalBits, class SegmentClass>

    /*
     * for a symbol_space
     */
    
    class symbol_space final {

      typedef SegmentClass segment_t;
      
      // Heap allocators derived from segment
      
      typedef typename segment_t::segment_manager segment_manager_t;
      typedef bip::basic_string<char,std::char_traits<char>, bip::allocator<char, segment_manager_t>> shared_string_t;
      typedef typename bip::allocator<void, segment_manager_t> void_allocator_t;

      // XXX UC
      typedef elemental_vector<std::size_t, segment_manager_t> elemental_vector_t;
      //typedef elemental_vector::index_t sparse_index_t;
      //typedef std::vector<std::size_t> sparse_index_t;


      /////////////////////////////////////////////////////////////////////
      // symbol - named vector with laziy computed elemental fingerprint
      //
      // indexed by: name hash, r&b tree for prefix of name, random access 

      struct symbol final {
        // symbol_t state
        shared_string_t _name;
        
      private:
        elemental_vector_t _basis; 

      public:

        // constructor
        symbol(const char* s, const std::vector<size_t>& fp, const void_allocator_t& void_alloc) : _name(s, void_alloc), _basis(fp, ElementalBits, void_alloc) {}
        
        symbol(const char* s, const void_allocator_t& void_alloc) : _name(s, void_alloc), _basis(ElementalBits, void_alloc) {}
        
        
        inline const std::string name(void) const {
          return std::string(_name.begin(), _name.end());
        }

        // printer
        friend std::ostream& operator<<(std::ostream& os, const symbol& s) {
          os << s._name;
          return os;
        }

      };

      
      // allocator for symbol
      
      typedef bip::allocator<symbol, segment_manager_t> symbol_allocator_t;
      
      // shared string helpers
      
      inline shared_string_t shared_string(const std::string& s) {
        return shared_string_t(s.c_str(), allocator);
      }

      inline shared_string_t shared_string(const char* s) {
        return shared_string_t(s, allocator); 
      }

      // partial (prefix) string comparison
      
      struct partial_string {
        partial_string(const shared_string_t& str) : str(str) {}
        shared_string_t str;
      };
      
      struct partial_string_comparator {
        bool operator()(const shared_string_t& x, const shared_string_t& y) const {
          return x<y;
        }

        bool operator()(const shared_string_t& x,const partial_string& y) const {
          return x.substr(0,y.str.size())<y.str;
        }

        bool operator()(const partial_string& x,const shared_string_t& y) const {
          return x.str<y.substr(0,x.str.size());
        }
      };


      // shared memory mapped multi index container type with it's indexes
      
      typedef multi_index_container<
        symbol,
        indexed_by<
          hashed_unique<BOOST_MULTI_INDEX_MEMBER(symbol, shared_string_t, _name)>,
          ordered_unique<BOOST_MULTI_INDEX_MEMBER(symbol, shared_string_t, _name), partial_string_comparator>,
          random_access<>
          >, symbol_allocator_t
        > symbol_table_t;


      
      ////////////////////////////////////////////////////
      /// vector_space implemented as a vector of vectors
      
      typedef VectorElementType element_t;
      
      typedef bip::allocator<element_t, segment_manager_t> element_allocator_t;
      
      typedef bip::vector<element_t, element_allocator_t> vector_t;
      
      
      /// the vector type
    public:
      struct vector final : public vector_t {

        /// construct fully 
        vector(const void_allocator_t& a) : vector_t(a) {
          this->reserve(VArraySize);
          #pragma unroll
          #pragma clang loop vectorize(enable) interleave(enable)
          for (std::size_t i = 0; i < VArraySize; ++i) this->push_back(0);
        }

        /// dimensions of this vector
        static const std::size_t dimensions =  VArraySize * sizeof(element_t) * CHAR_BITS;

        //friend std::ostream& operator<<(std::ostream& os, const vector& v) {
        //  os << v.something const; 
        //  return os;
        //}

                
        /* set all bits */

        inline void ones(void) {
        #pragma unroll
        #pragma clang loop vectorize(enable) interleave(enable)
          for (std::size_t i=0; i < VArraySize; ++i) {
            (*this)[i] = -1; 
          }
        }

        /* clear all bits */

        inline void zeros(void) {
        #pragma unroll
        #pragma clang loop vectorize(enable) interleave(enable)
          for (std::size_t i=0; i < VArraySize; ++i) {
            (*this)[i] = 0; 
          }
        }

        
        // vector properties
        
        inline const std::size_t count() {
          std::size_t count = 0;
          #pragma unroll
          #pragma clang loop vectorize(enable) interleave(enable)
          for (std::size_t i=0; i < VArraySize; ++i) {
          #if VELEMENT_64
            count += __builtin_popcountll((*this)[i]);
          #else
            count += __builtin_popcount((*this)[i]);
          #endif
          }
          return count;
        }

        
        inline double density() {
          return (double) count() / dimensions;
        }


        // vector measurements
        
        inline const std::size_t distance(const vector& v) {
          std::size_t distance = 0;
          #pragma unroll
          #pragma clang loop vectorize(enable) interleave(enable)
          for (std::size_t i=0; i < VArraySize; ++i) {
            element_t r = (*this)[i] ^ v[i]; 
          #ifdef VELEMENT_64
            distance += __builtin_popcountll(r);
          #else
            distance += __builtin_popcount(r);
          #endif
          }
          return distance;
        }


        inline const std::size_t inner(const vector& v) {
          std::size_t count = 0;
          #pragma unroll
          #pragma clang loop vectorize(enable) interleave(enable)
          for (std::size_t i=0; i < VArraySize; ++i) {
            element_t r = (*this)[i] & v[i]; 
          #ifdef VELEMENT_64
            count += __builtin_popcountll(r);
          #else
            count += __builtin_popcount(r);
          #endif
          }
          return count;
        }


        inline std::size_t countsum(const vector& v) {
          std::size_t count = 0;
          #pragma unroll
          #pragma clang loop vectorize(enable) interleave(enable)
          for (std::size_t i=0; i < VArraySize; ++i) {
            element_t r = (*this)[i] | v[i]; 
          #ifdef VELEMENT_64
            count += __builtin_popcountll(r);
          #else
            count += __builtin_popcount(r);
          #endif
          }
          return count;
        }

        /// Similarity of vectors 
        inline double similarity(const vector& v) {
          // inverse of the normalized distance
          return 1.0 - (double) distance(v)/(VArraySize * sizeof(element_t) * CHAR_BITS);
        }



        /////////////////////////////////
        // basic operations on vectors //
        /////////////////////////////////

        /* add or superpose */

        inline void superpose(const vector& v) {
        #pragma unroll
        #pragma clang loop vectorize(enable) interleave(enable)
          for (std::size_t i=0; i < VArraySize; ++i) {
            (*this)[i] |= v[i];
          }
        }


        
        // set a list of bits
        inline void setbits(const std::vector<std::size_t>::iterator& start,
                            const std::vector<std::size_t>::iterator& end) {
          for (auto it = start; it < end; ++it){
            std::size_t r = *it;
            std::size_t i = r / (sizeof(element_t) * CHAR_BITS);
            std::size_t b = r % (sizeof(element_t) * CHAR_BITS);
            (*this)[i] |= (ONE << b);
          }
        }

        
        /* subtract v from u */

        inline void subtract(const vector& v) {
        #pragma unroll
        #pragma clang loop vectorize(enable) interleave(enable)
          for (std::size_t i=0; i < VArraySize; ++i) {
            (*this)[i] &= ~v[i];
          }
        }

        inline void multiply(const vector& v) {
        #pragma unroll
        #pragma clang loop vectorize(enable) interleave(enable)
          for (std::size_t i=0; i < VArraySize; ++i) {
            (*this)[i] ^= v[i];
          }
        }
        
      };

    private:
      // vector allocators
      
      typedef bip::allocator<vector, segment_manager_t> vector_allocator_t;

      // vector of vectors
      
      typedef bip::vector<vector, vector_allocator_t> vector_vector_t;

      
      // vector_space is_a vector_vector_t
      
      struct vector_space final : public vector_vector_t {

        vector_space(const void_allocator_t& a) : vector_vector_t(a) {}

        // c'tor with reservation size
        vector_space(const::size_t n, const void_allocator_t& a) : vector_vector_t(a) {
          this->reserve(n);
        }
        
      };

      
      typedef vector_space vector_space_t;

      
      ///////////////////////////////////
      // symbol_space public interface //
      ///////////////////////////////////
      
    public:

      // constructor pre-allocate cache space
            
      symbol_space(const std::string& s, const::size_t n, segment_t& m)
        : name(s), segment(m), allocator(segment.get_segment_manager()) {
        // create vector_space
        std::string vs_name = "__" + name;
        vectors = segment.template find_or_construct<vector_space_t>(vs_name.c_str())(n, allocator);
        // ensure multi_index container is constructed: this is the symbol space
        index = segment.template find_or_construct<symbol_table_t>(name.c_str())(allocator);
      }

      // default cache space
      
      symbol_space(const std::string& s, segment_t& m)
        : name(s), segment(m), allocator(segment.get_segment_manager()) {
        // create vector space
        std::string vs_name = "__" + name;
        vectors = segment.template find_or_construct<vector_space_t>(vs_name.c_str())(allocator);
        // ensure multi_index container is constructed: this is the symbol space
        index = segment.template find_or_construct<symbol_table_t>(name.c_str())(allocator);
     }

      
      // destructor
      /*
      ~symbol_space() {
        // should we remove the shared_memory_object (by name) here as well?
        // segment is global so flushing should be manged by owner...
      }
      */
      // delete the rest of the gang don't ever want to copy a space -- but move?

      symbol_space(const symbol_space&) = delete;
      symbol_space(symbol_space&&) = delete;
      const symbol_space& operator=(const symbol_space&) = delete;
      const symbol_space& operator=(symbol_space&&) = delete;

      
      // printer symbol_space stats
      
      friend std::ostream& operator<<(std::ostream& os, symbol_space& t) {
        os << t.spacename() << " #" << t.entries(); 
        return os;
      }  
     

      typedef symbol symbol; // public face of symbol
      typedef vector vector; //xx
    
      
      /////////////////////////////////////
      /// multi index container indexes ///
      /////////////////////////////////////
      
      typedef std::pair<typename symbol_table_t::iterator, bool> inserted_t;
      
      /// insert

      inserted_t insert(const std::string& k) {
        inserted_t p = index->insert(symbol(k.c_str(), allocator));
        if (p.second) {
          // inserted: string iterator maps to index
          // call vector allocator
          vectors->push_back(vector(allocator));
          //assert (vectors->size()-1 == n2i(p.first));
        }
        return p;
      }

      inserted_t insert(const std::string& k, const std::vector<std::size_t>& fp) {
        inserted_t p = index->insert(symbol(k.c_str(), fp, allocator));
        if (p.second) {
          // inserted: string iterator maps to index
          // call vector allocator
          vectors->push_back(vector(allocator));
          //assert (vectors->size()-1 == n2i(p.first));
        }
        return p;
      }
      
      /// random access index
      
      typedef typename symbol_table_t::template nth_index<2>::type symbol_by_index;
      
      /// overload [] and delegate
      
      inline const symbol& operator[](std::size_t i) {
        symbol_by_index& symbols = index->template get<2>(); 
        return symbols[i]; 
      }

      
      /// lookup by name
      
      typedef typename symbol_table_t::template nth_index<0>::type symbol_by_name;

      inline boost::optional<const symbol&> get_symbol_by_name(const std::string& k) {
        symbol_by_name& name_idx = index->template get<0>();
        typename symbol_by_name::iterator i = name_idx.find(shared_string(k));
        if (i == name_idx.end()) return boost::none;
        else return *i;
      }

      /// xxx under construction: expose vectors directly
      
      inline boost::optional<vector&> get_vector_by_name(const std::string& k) {
        symbol_by_name& name_idx = index->template get<0>();
        typename symbol_by_name::iterator it = name_idx.find(shared_string(k));
        if (it == name_idx.end()) return boost::none;
        // TODO look at n2i efficiency
        else return (*vectors)[(n2i(it))];
      }

      
      /// search by name prefix
      
      typedef typename symbol_table_t::template nth_index<1>::type symbol_by_prefix;  
      typedef typename symbol_by_prefix::iterator symbol_iterator;

      inline std::pair<symbol_iterator, symbol_iterator> search(const std::string& k) {
        symbol_by_prefix& name_idx = index->template get<1>();
        return name_idx.equal_range(partial_string(shared_string(k)));
      }

      /// project the iterator into direct index
      
      inline const std::size_t n2i(typename symbol_by_name::iterator& nit) {
        return ((index->template project<2>(nit)) - (index->template get<2>().begin()));
      }


      ///
      /* 
         WIP: parallelised SIMD operations on entire vectorspace
         1. distribute by segments (n_cores) on cpu (treat as separate arrays on gpu?) 
         2. accumulate number of matching targets in parallel scan
         3. allocate smallest set of scores and sort in main thread 
      */
      
      /// point
      
      struct point {
    
        std::string name;
        double similarity;
        double density;
        
        //
        explicit point(const std::string& v, const double s, const double d)
          : name(v), similarity(s), density(d) {}

        /// similarity is 1-d
        /// comparison for descending similarity
        bool operator< (const point& s) const {
          return similarity > s.similarity;
        }

        bool operator==(const point& s) const {
          return name == s.name && similarity == s.similarity;
        }
        bool operator!=(const point& s) const {
          return name != s.name || similarity != s.similarity;
        }

        friend std::ostream& operator<<(std::ostream& os, point& p) {
          os <<  p.name << ":" << p.similarity << ":" << p.density;
          return os;
        }  

      };
      
      //////////////////////
      /// computed topology
      
      typedef std::vector<point> topology;

      ///////////////////////////////////////
      /// compute neighbourhood of a vector
      
      inline const topology neighbourhood(const vector& u,
                                          const double p,
                                          const double d,
                                          const std::size_t n) {
        
        const std::size_t m = vectors->size();
        // allocate working memory - TODO try this on stack
        auto work = new double[m*2];
        
        //// parallel block ////
        #ifdef HAVE_DISPATCH
        dispatch_queue_t queue = dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_HIGH, 0);
        dispatch_apply(m, queue, ^(std::size_t i) {
            work[i*2] = (*vectors)[i].density();
            work[i*2+1] = (*vectors)[i].similarity(u);
          });
        // XXXX ARC forbids release XXXXX dispatch_release(queue);
        
        #else
        #pragma omp parallel for 
        for (std::size_t i=0; i < m; ++i) {
          work[i*2] = (*vectors)[i].density();
          work[i*2+1] = (*vectors)[i].similarity(u);
        }
        #endif
        //// end parallel block ////
      
        std::vector<point> topo;
        topo.reserve(m); // ??? hmm is there a statistic here?
        
        // filter work array
        for (std::size_t i=0; i < m; ++i) {
          double rho = work[i*2];
          double sim = work[i*2+1];
          // apply p-d-filter
          if (rho <= d && sim >= p) {
            topo.push_back(point((*this)[i].name(), sim, rho));
          }
        }
        
        delete[] work;
        // sort the scores in similarity order 
        sort(topo.begin(), topo.end());
        const std::size_t ns = topo.size();
        
        // chop off (long?) tail before serialising
        topo.erase(topo.begin() + ((n < ns) ? n : ns), topo.end()); 
        return topo;
      }
  
      /// delegated space iterators

      typedef typename symbol_table_t::iterator iterator;
      typedef typename symbol_table_t::const_iterator const_iterator;

      //inline iterator begin() { return index->begin(); }
      //inline iterator end() { return index->end(); }

      inline const_iterator begin() { return index->begin(); }
      inline const_iterator end() { return index->end(); }

      // delegated properties
      inline const size_t entries() { return index->size(); }
      inline const std::string spacename() const { return name; }

      
      // TODO: shrink_to_fit, grow
      
    private:    

      std::string          name; 
      symbol_table_t*      index;
      vector_space*        vectors; 
      segment_t&           segment;
      void_allocator_t     allocator;
    };
  }
}}
