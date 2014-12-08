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

namespace bip = boost::interprocess;

namespace gecko {

  namespace symtab {

    // todo create a symbol struct rather than the pair
    class table {

      
      typedef bip::basic_string<
        char,
        std::char_traits<char>,
        bip::allocator<char, bip::managed_mapped_file::segment_manager>> shared_string_t;
      
      typedef int id_t;

      typedef typename std::pair<shared_string_t, id_t> val_t;

      typedef bip::managed_mapped_file segment_t;
      typedef segment_t::segment_manager segment_manager_t;

      typedef bip::allocator<val_t, segment_manager_t> allocator_t;

      // tags
      struct from {};
      struct to {};

    public:   
      typedef boost::multi_index_container<
        val_t,
        boost::multi_index::indexed_by<
          boost::multi_index::hashed_unique<
            boost::multi_index::tag<from>,
            boost::multi_index::member<val_t, shared_string_t, &val_t::first>>,
          boost::multi_index::hashed_unique<
            boost::multi_index::tag<to>,
            boost::multi_index::member<val_t, id_t, &val_t::second>>>,
        allocator_t> map_t;


      
    public:
      
      table(const char* name, const size_t size) : segment(boost::interprocess::open_or_create, name, size),
                                                   allocator(segment.get_segment_manager()) {
        map = segment.find_or_construct<map_t>(name)(allocator);
      }

      /*
      void insert(map_t::value_type v) {
        map->insert(v);
      }
      */
      inline void insert(const std::string& k, const id_t& i) {
        shared_string_t symbol(k.c_str(), shared_string_t::allocator_type(segment.get_segment_manager()));
        map->insert(map_t::value_type(symbol, i));
      }

      map_t* get_map() {
        return map;
      }

      ~table() {
        // should we remove the shared_memory_object (by name) here as well?
        segment.flush();
      }
      
    private:
      map_t* map;
      segment_t segment;
      allocator_t allocator;
    };
    
  }
}
