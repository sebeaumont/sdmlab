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

#include "symbolic_vector.hpp"

using boost::multi_index_container;
using namespace boost::multi_index;
namespace bip = boost::interprocess;

namespace gecko {

  namespace vspace {

    typedef bip::managed_mapped_file segment_t;

    // memory mapped file based symbol table
    template <typename T, std::size_t N, std::size_t S, typename A> 
    class vectorspace {
         
      // symbol type and allocator -- can we make the vector type fully parametric V<T,N,S,A> ?
      typedef gs::vector<T,N,S,A> vector_t;
      typedef bip::allocator<vector_t, segment_manager_t> vector_allocator_t;


      
      // shared string helpers -- need to go in symbol.hpp
      
      inline shared_string_t shared_string(const std::string& s) {
        return shared_string_t(s.c_str(), shared_string_t::allocator_type(allocator));  
      }

      inline shared_string_t shared_string(const char* s) {
        return shared_string_t(s, shared_string_t::allocator_type(allocator));  
      }

      struct partial_string {
        partial_string(const shared_string_t& str) : str(str) {}
        shared_string_t str;
      };

      // need shared_string_t from symbol_t
      
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
        db->insert(symbol_t(k.c_str(), allocator));
      }

      
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

      // delegated space iterators

      typedef typename symbol_space_t::iterator iterator;
      typedef typename symbol_space_t::const_iterator const_iterator;

      inline iterator begin() { return db->begin(); }
      inline iterator end() { return db->end(); }

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
