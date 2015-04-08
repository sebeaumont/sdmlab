#pragma once
#include <boost/interprocess/containers/string.hpp>
#include <boost/interprocess/containers/vector.hpp>
#include <boost/multi_index_container.hpp>
#include <boost/multi_index/ordered_index.hpp>
#include <boost/multi_index/hashed_index.hpp>
#include <boost/multi_index/random_access_index.hpp>
#include <boost/multi_index/member.hpp>
#include <boost/optional.hpp>

#include <elemental_space.hpp>

namespace gecko {

  namespace vspace {

    using boost::multi_index_container;
    using namespace boost::multi_index;
    namespace bip = boost::interprocess;
    
    // feature_space
    
    template <typename T, std::size_t N, std::size_t S, class A> 
    class feature_space {
      
      typedef A segment_t;
      typedef typename segment_t::segment_manager segment_manager_t;
      typedef bip::basic_string<char,std::char_traits<char>,
                                bip::allocator<char, segment_manager_t>> shared_string_t;
      typedef typename bip::allocator<void, segment_manager_t> void_allocator_t;

      // bit vector types

      typedef binary_vector<T, segment_manager_t> bitv_vector_t;

      // symbol and vector types
      
      struct feature_vector {

        enum status_t {NEW, USED, OLD, FREE}; // TODO mainly for GC
      
        // data
        shared_string_t name;
        status_t flags;
        bitv_vector_t super;
        bitv_vector_t suber;
        bitv_vector_t semv;

        // constructor
        feature_vector(const char* s, const void_allocator_t& void_alloc)
          : name(s, void_alloc), flags(NEW), super(S/2, 0, void_alloc), suber(S/2, 0, void_alloc), semv(N, 0, void_alloc) {}
      
        // printer
        friend std::ostream& operator<<(std::ostream& os, const feature_vector& s) {
          os << "<" << s.name << ", " << s.flags << "," << S << "," << s.semv.size() * sizeof(T) * 8 << ">";
          return os;
        }

        // superposition
        feature_vector& operator+=(const feature_vector& v) {
          // add/or the super bits from v, subtract/nand suber bits from v
          return *this;
        }

        // measurement
        std::size_t difference(const feature_vector& v) const {
          // popcount u xor v
          return 0;
        }
        
      };

      
      // allocator for symbol
      typedef bip::allocator<feature_vector, segment_manager_t> vector_allocator_t;
      
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
        feature_vector,
        indexed_by<
          hashed_unique<BOOST_MULTI_INDEX_MEMBER(feature_vector, shared_string_t, name)>,
          ordered_unique<BOOST_MULTI_INDEX_MEMBER(feature_vector, shared_string_t, name), partial_string_comparator>,
          random_access<>
          >, vector_allocator_t
        > vector_space_t;

      
    public:
      
      // the segment is a global memory mapped file we need to share
      // this across multiple namespaces that is not enforced here but
      // relies on caller doing so TODO factory to do this...
            
      feature_space(const std::string& s, segment_t& m)
        : name(s.c_str()), segment(m), allocator(segment.get_segment_manager()) {
        // ensure multi_index container is constructed: this is the symbol space
        db = segment.template find_or_construct<vector_space_t>(name)(allocator);
      }

      
      ~feature_space() {
        // should we remove the shared_memory_object (by name) here as well?
        // segment is global so flushing should be manged by owner... 
        segment.flush();
      }
      
      
      // delete the rest of the gang don't ever want to copy a space -- but move?

      feature_space(const feature_space&) = delete;
      feature_space(feature_space&&) = delete;
      const feature_space& operator=(const feature_space&) = delete;
      const feature_space& operator=(feature_space&&) = delete;

      // printer feature_space stats
      
      friend std::ostream& operator<<(std::ostream& os, feature_space& t) {
        os << t.spacename() << " #" << t.entries(); 
        return os;
      }  

      // vector is the public type of space elements
      
      typedef feature_vector vector;

      // insertion
      
      inline void insert(const std::string& k) {
        db->insert(feature_vector(k.c_str(), allocator));
      }

      // random access by size_t
      typedef typename vector_space_t::size_type space_size_t;
      typedef typename vector_space_t::template nth_index<2>::type vector_by_index;
      
      // overload [] and delegate
      
      inline const vector& operator[](std::size_t i) {
        vector_by_index& vectors = db->template get<2>(); 
        return vectors[i]; 
      }
      
      // lookup by name
      
      typedef typename vector_space_t::template nth_index<0>::type vector_by_name;

      inline boost::optional<const feature_vector&> get(const std::string& k) {
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
      inline const char* spacename() const { return name; }

      // TODO: shrink_to_fit, grow
      
    private:    
      const char*          name; 
      vector_space_t*      db;
      segment_t&           segment;
      void_allocator_t     allocator;
  
    };
  }
}
