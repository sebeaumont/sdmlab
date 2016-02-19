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

namespace sdm {

  namespace mms {

    using boost::multi_index_container;
    using namespace boost::multi_index;
    namespace bip = boost::interprocess;
    
    // feature_space
    
    template <typename T, std::size_t N, std::size_t S, class A> 
    class symbol_space {
      
      typedef A segment_t;
      typedef typename segment_t::segment_manager segment_manager_t;
      typedef bip::basic_string<char,std::char_traits<char>, bip::allocator<char, segment_manager_t>> shared_string_t;
      typedef typename bip::allocator<void, segment_manager_t> void_allocator_t;

      // vector types

      typedef elemental_vector<T, segment_manager_t> elemental_vector_t;
      typedef std::size_t semantic_vector_t; //just an index

      // symbolic vector
      
      struct symbol_vector {

        enum status_t {NEW, USED, OLD, FREE}; // TODO mainly for GC
      
        // symbol_vector state
        shared_string_t name;
        status_t flags;
        /*
         * UC: the semantic vector can be an index into the search
         * space i.e. std::size_t, the elemental vectors could be a
         * compact list of bits to set/clear given loop unrolling when
         * superposing...  Search space of semantic vectors for this
         * could be passed an alternate allocator/segment...
         */
        elemental_vector_t super;
        elemental_vector_t suber;
        semantic_vector_t semv;

        // constructor
        symbol_vector(const char* s, const void_allocator_t& void_alloc)
          : name(s, void_alloc), flags(NEW), super(S/2, 0, void_alloc), suber(S/2, 0, void_alloc), semv(-1) {}
      
        // printer
        friend std::ostream& operator<<(std::ostream& os, const symbol_vector& s) {
          os << s.name << " (" << s.flags << "," << S << "," << s.semv << ")";
          return os;
        }

        // superposition
        symbol_vector& operator+=(const symbol_vector& v) {
          // add/or the super bits from v, subtract/nand suber bits from v
          return *this;
        }

        // measurement
        std::size_t difference(const symbol_vector& v) const {
          // popcount u xor v
          return 0;
        }
        
      };

      
      // allocator for symbol
      typedef bip::allocator<symbol_vector, segment_manager_t> vector_allocator_t;
      
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
        symbol_vector,
        indexed_by<
          hashed_unique<BOOST_MULTI_INDEX_MEMBER(symbol_vector, shared_string_t, name)>,
          ordered_unique<BOOST_MULTI_INDEX_MEMBER(symbol_vector, shared_string_t, name), partial_string_comparator>,
          random_access<>
          //hashed_unique<BOOST_MULTI_INDEX_MEMBER(symbol_vector, semantic_vector_t, semv)>
          >, vector_allocator_t
        > vector_space_t;

      
    public:
      
      // the segment is a global memory mapped file we need to share
      // this across multiple namespaces that is not enforced here but
      // relies on caller doing so TODO factory to do this...
            
      symbol_space(const std::string& s, segment_t& m)
        : name(s), segment(m), allocator(segment.get_segment_manager()) {
        // ensure multi_index container is constructed: this is the symbol space
        db = segment.template find_or_construct<vector_space_t>(name.c_str())(allocator);
      }

      
      ~symbol_space() {
        // should we remove the shared_memory_object (by name) here as well?
        // segment is global so flushing should be manged by owner... 
        segment.flush();
      }
      
      
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

      // vector is the public type of space elements
      
      typedef symbol_vector vector;

      // insertion
      
      inline void insert(const std::string& k) {
        db->insert(symbol_vector(k.c_str(), allocator));
      }

      
      // random access
      
      //typedef typename vector_space_t::size_type space_size_t;
      typedef typename vector_space_t::template nth_index<2>::type vector_by_index;
      
      // overload [] and delegate
      
      inline const vector& operator[](std::size_t i) {
        vector_by_index& vectors = db->template get<2>(); 
        return vectors[i]; 
      }

      
      // lookup by name
      
      typedef typename vector_space_t::template nth_index<0>::type vector_by_name;

      inline boost::optional<const symbol_vector&> get(const std::string& k) {
        vector_by_name& name_idx = db->template get<0>();
        typename vector_by_name::iterator i = name_idx.find(shared_string(k));
        if (i == name_idx.end()) return boost::none;
        else return *i;
      }


      // search by prefix
      
      typedef typename vector_space_t::template nth_index<1>::type vector_by_prefix;  
      typedef typename vector_by_prefix::iterator vector_iterator;

      inline std::pair<vector_iterator, vector_iterator> search(const std::string& k) {
        vector_by_prefix& name_idx = db->template get<1>();
        return name_idx.equal_range(partial_string(shared_string(k)));
      }


      // delegated space iterators

      typedef typename vector_space_t::iterator iterator;
      typedef typename vector_space_t::const_iterator const_iterator;

      //inline iterator begin() { return db->begin(); }
      //inline iterator end() { return db->end(); }

      inline const_iterator begin() { return db->begin(); }
      inline const_iterator end() { return db->end(); }

      // delegated properties
      inline const size_t entries() { return db->size(); }
      inline const std::string spacename() const { return name; }

      // TODO: shrink_to_fit, grow
      
    private:    
      std::string          name; 
      vector_space_t*      db;
      segment_t&           segment;
      void_allocator_t     allocator;
  
    };
  }
}
