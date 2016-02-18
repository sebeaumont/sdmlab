#pragma once
/***************************************************************************
 * symbolic_space.hpp - symbol table container 
 *
 * Copyright (c) Simon Beaumont 2012-2016 - All Rights Reserved.
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


namespace sdm {

  namespace mms {
    
    using boost::multi_index_container;
    using namespace boost::multi_index;
    namespace bip = boost::interprocess;

    template <class A> 
    class symbol_space {

      typedef A segment_t;

      typedef typename segment_t::segment_manager segment_manager_t;

      typedef bip::basic_string<char,std::char_traits<char>,
                                bip::allocator<char, segment_manager_t>> shared_string_t;
      
      typedef typename bip::allocator<void, segment_manager_t> void_allocator_t;

      // symbol space element
      
      struct symbol {

        enum status_t {NEW, USED, OLD, FREE}; // TODO mainly for GC
      
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

      // common code for index maintenance
      
      typedef symbol symbol_t;
      
      // allocator for symbol

      typedef bip::allocator<symbol_t, segment_manager_t> symbol_allocator_t;
      
      // shared string helpers
      
      inline shared_string_t shared_string(const std::string& s) {
        return shared_string_t(s.c_str(), allocator);
      }

      inline shared_string_t shared_string(const char* s) {
        return shared_string_t(s, allocator); 
      }

      // symbol name prefix search
      
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
      // TODO - be nice to generate ids for lexica only
      
      typedef multi_index_container<
        symbol_t,
        indexed_by<
          hashed_unique<BOOST_MULTI_INDEX_MEMBER(symbol_t, shared_string_t, name)>,
          ordered_unique<BOOST_MULTI_INDEX_MEMBER(symbol_t, shared_string_t, name), partial_string_comparator>
          >, symbol_allocator_t
        > symbol_space_t;

      
    public:
      
      symbol_space(const std::string& s, segment_t& m)
        : name(s.c_str()), segment(m), allocator(segment.get_segment_manager()) {
        // ensure multi_index container is constructed: this is the symbol space
        db = segment.template find_or_construct<symbol_space_t>(name)(allocator);
      }

      ~symbol_space() {
        // should we remove the shared_memory_object (by name) here as well?
        // segment is global so flushing should be manged by owner... 
        segment.flush();
        std::cerr << this << " was flushed" << std::endl; 
      }

      // delete the rest of the gang don't ever want to copy a space -- but move?

      symbol_space(const symbol_space&) = delete;
      symbol_space(symbol_space&&) = delete;
      const symbol_space& operator=(const symbol_space&) = delete;
      const symbol_space& operator=(symbol_space&&) = delete;

      // printer give global segment statistics as well as symbol_space specifics

      friend std::ostream& operator<<(std::ostream& os, symbol_space& t) {
        os << "(" << (t.check_sanity() ? ":-) " : ":-( ") << t.spacename()
           << "[" << t.entries()
           << "] U: " <<  (float) (t.size() - t.get_free()) / (1024 * 1024)
           << " F: " << (float) t.get_free() / (1024*1024)
           << " T: " << (float) t.size() / (1024*1024) << ")";
        return os;
      }  

      // insert symbol into space
      
      inline void insert(const std::string& k) {
        db->insert(symbol_t(k.c_str(), allocator));
      }

      // lookup by name
      
      typedef typename symbol_space_t::template nth_index<0>::type symbol_by_name;

      inline boost::optional<const symbol_t&> get(const std::string& k) {
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

      // the public type of a record
      typedef symbol_t vector_t;

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

      // data
    private:    
      const char*          name; 
      symbol_space_t*      db;
      segment_t&           segment;
      void_allocator_t     allocator;
    };
  }
}

