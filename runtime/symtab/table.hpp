//
//  table.hpp
//  gecko symbol table
//
//  Created by Simon Beaumont on 07/12/2014.
//  Copyright (c) 2014 Simon Beaumont. All rights reserved.
//

#include <iostream>

#include <boost/interprocess/allocators/allocator.hpp>
#include <boost/interprocess/managed_mapped_file.hpp>
#include <boost/interprocess/containers/string.hpp>

#include <boost/multi_index_container.hpp>
#include <boost/multi_index/ordered_index.hpp>
#include <boost/multi_index/hashed_index.hpp>
#include <boost/multi_index/member.hpp>

using boost::multi_index_container;
using namespace boost::multi_index;
namespace bip = boost::interprocess;

namespace gecko {

  namespace symtab {

    //TODO  2. dynamic growth of the segment
    
    class table {

      typedef bip::basic_string<
        char,
        std::char_traits<char>,
        bip::allocator<char, bip::managed_mapped_file::segment_manager>> shared_string_t;
      
      typedef std::size_t id_t;

      enum status_t {NEW, USED, OLD, FREE};
      
      struct symbol {
        shared_string_t name;
        id_t id;
        status_t flags;
        
        symbol(const shared_string_t& s, const id_t& i) : name(s), id(i), flags(NEW) {}

        friend std::ostream& operator<<(std::ostream& os, const symbol& s) {
          os << "(" << s.name << ", " << s.id << ", " << s.flags << ")" << std::endl;
          return os;
        }
      };
      
      typedef bip::managed_mapped_file segment_t;
      typedef segment_t::segment_manager segment_manager_t;
      typedef bip::allocator<symbol, segment_manager_t> allocator_t;
      
      typedef multi_index_container<symbol,
                                    indexed_by<
                                      hashed_unique<BOOST_MULTI_INDEX_MEMBER(symbol, shared_string_t, name)>,
                                      hashed_unique<BOOST_MULTI_INDEX_MEMBER(symbol, id_t, id)>
                                      >, allocator_t
                                    > symbol_table_t;
      
      
    public:
      
      table(const char* s, const size_t size) : name(s),
                                                segment(boost::interprocess::open_or_create, name, size),
                                                allocator(segment.get_segment_manager()) {
        db = segment.find_or_construct<symbol_table_t>(name)(allocator);
      }


      inline void insert(const std::string& k, const id_t& i) {
        shared_string_t sym(k.c_str(), shared_string_t::allocator_type(segment.get_segment_manager()));
        db->insert(symbol(sym, i));
      }


      ~table() {
        // should we remove the shared_memory_object (by name) here as well?
        segment.flush();
      }
      
      // rather do this

      typedef symbol_table_t::iterator iterator;
      typedef symbol_table_t::const_iterator const_iterator;
      inline iterator begin() { return db->begin(); }
      inline iterator end() { return db->end(); }
      
      
    private:    
      const char *name; 
      symbol_table_t* db;
      segment_t segment;
      allocator_t allocator;
    };
    
  }
}
