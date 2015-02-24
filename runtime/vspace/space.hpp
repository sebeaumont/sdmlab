/***************************************************************************
 * space.hpp - symbol table container  (part of the Gecko qdsm environment)
 *
 * Copyright (c) Simon Beaumont 2012-2014 - All Rights Reserved.
 * See: LICENSE for conditions under which this software is published.
 ***************************************************************************/

#include <iostream>
#include <boost/interprocess/allocators/allocator.hpp>
#include <boost/interprocess/managed_mapped_file.hpp>
#include <boost/interprocess/containers/string.hpp>
#include <boost/interprocess/containers/vector.hpp>
#include <boost/multi_index_container.hpp>
#include <boost/multi_index/ordered_index.hpp>
#include <boost/multi_index/hashed_index.hpp>
#include <boost/multi_index/member.hpp>
#include <boost/optional.hpp>


using boost::multi_index_container;
using namespace boost::multi_index;
namespace bip = boost::interprocess;

namespace gecko {

  namespace vspace {

    typedef bip::managed_mapped_file segment_t;

    // memory mapped file based symbol table
    template <typename T, std::size_t N, std::size_t S> 
    class space final {
      
      // managed segments and allocator types

      typedef segment_t::segment_manager                       segment_manager_t;

      typedef bip::allocator<void, segment_manager_t>          void_allocator_t;
      
      // strings allocated in the segment
      typedef bip::basic_string<char, std::char_traits<char>, bip::allocator<char, segment_manager_t>> shared_string_t;
      
      // vectors of int allocated in the segment
      typedef bip::allocator<T, segment_manager_t>             bitv_allocator_t;
      typedef bip::vector<T, bitv_allocator_t>                 bitv_vector_t;
      typedef bip::allocator<bitv_vector_t, segment_manager_t> bitv_vector_allocator_t;

      // status 
      enum status_t {NEW, USED, OLD, FREE}; // TODO mainly for GC

      // symbols stored in the shared multi_index container

      // TODO feasibility of several types of symbol: semantic vectors, elemental vectors and joint
      // to ease storage
      
      struct symbol final {
        
        shared_string_t name;
        status_t flags;
        bitv_vector_t semv;
        bitv_vector_t elev;

        // construct a symbol in the space
        
        symbol(const char* s, const void_allocator_t& void_alloc)
          : name(s, void_alloc), flags(NEW), semv(N, 0, void_alloc), elev(S, 0, void_alloc) {}
        
        // symbol printer
        friend std::ostream& operator<<(std::ostream& os, const symbol& s) {
          os << "(" << s.name << ", " << s.flags << ", "
             << s.semv.size() << ", " << s.elev.size() << ")";
          return os;
        }

        
      public:

        void superpose(const symbol& other) {
          #pragma clang loop vectorize(enable) interleave(enable)
          for (std::size_t i = 0; i < N; ++i) {
            semv[i] |= other.elev[i];
          }
        }

        // ... under construction
      };

            
      // symbol allocator
   
      typedef bip::allocator<symbol, segment_manager_t> symbol_allocator_t;

      // partial strings XXX I wonder if this could be more efficient than having to create
      // shared strings and partial strings by comparing shared strings with regular strings
      
      struct partial_string {
        partial_string(const shared_string_t& str) : str(str) {}
        shared_string_t str;
      };

      struct partial_string_less {
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
          hashed_unique<BOOST_MULTI_INDEX_MEMBER(symbol, shared_string_t, name)>,
          ordered_unique<BOOST_MULTI_INDEX_MEMBER(symbol, shared_string_t, name), partial_string_less>
          >, symbol_allocator_t
        > symbol_space_t;

      
      // shared string helpers
      
      inline shared_string_t shared_string(const std::string& s) {
        return shared_string_t(s.c_str(), allocator);//shared_string_t::allocator_type(allocator));  
      }

      inline shared_string_t shared_string(const char* s) {
        return shared_string_t(s, allocator);//shared_string_t::allocator_type(allocator));  
      }

      
    public:
      
      // the segment is a global memory mapped file we need to share
      // this across multiple namespaces that is not enforced here but
      // relies on caller doing so TODO factory to do this...
            
      space(const std::string& s, segment_t& m)
        : name(s.c_str()), segment(m), allocator(segment.get_segment_manager()) {
        // ensure multi_index container is constructed: this is the symbol space
        db = segment.find_or_construct<symbol_space_t>(name)(allocator);
      }


      ~space() {
        // should we remove the shared_memory_object (by name) here as well?
        // segment is global so flushing should be manged by owner... 
        segment.flush();
      }

      // delete the rest of the gang don't ever want to copy a space -- but move?

      space(const space&) = delete;
      space(space&&) = delete;
      const space& operator=(const space&) = delete;
      const space& operator=(space&&) = delete;

      // printer give global segment statistics as well as space specifics

      friend std::ostream& operator<<(std::ostream& os, space& t) {
        os << "(" << (t.check_sanity() ? ":-) " : ":-( ") << t.spacename()
           << "[" << t.entries()
           << "] U: " <<  (float) (t.size() - t.get_free()) / (1024 * 1024)
           << " F: " << (float) t.get_free() / (1024*1024)
           << " T: " << (float) t.size() / (1024*1024) << ")";
        return os;
      }  

      // insertion
      
      inline void insert(const std::string& k) {
        db->insert(symbol(k.c_str(), allocator));
      }

      
      // xxx not sure how expensive optional values are at runtime
      

      // lookup by name
      typedef typename symbol_space_t::template nth_index<0>::type symbol_by_name;

      inline boost::optional<const symbol&> get(const std::string& k) {
        symbol_by_name& name_idx = db->template get<0>();
        typename symbol_by_name::iterator i = name_idx.find(shared_string(k));
        if (i == name_idx.end()) return boost::none;
        else return *i;
      }


      // search by prefix
      typedef typename symbol_space_t::template nth_index<1>::type symbol_by_prefix;  
      typedef typename symbol_by_prefix::iterator symbol_iterator;

      inline std::pair<symbol_iterator, symbol_iterator> search(const std::string& k) {
        symbol_by_prefix& name_idx = db->template get<1>();
        return name_idx.equal_range(partial_string(shared_string(k)));
      }

      // delegated iterators

      typedef typename symbol_space_t::iterator iterator;
      typedef typename symbol_space_t::const_iterator const_iterator;

      inline iterator begin() { return db->begin(); }
      inline iterator end() { return db->end(); }

      // symbol name index
      //inline symbol_by_prefix& index() { return db->template get<1>(); } 

      // symbol implements a vector in the api
      typedef symbol vector;
      
      // delegated properties
      
      inline const size_t size() { return segment.get_size(); }
      inline const size_t get_free() { return segment.get_free_memory(); }
      inline const bool check_sanity() { return segment.check_sanity(); }
      inline const size_t entries() { return db->size(); }
      inline const char* spacename() const { return name; }

      // TODO: shrink_to_fit, grow
      
    private:    
      const char*          name; 
      symbol_space_t*      db;
      segment_t&           segment;
      void_allocator_t     allocator;
    };
    
  }
}
