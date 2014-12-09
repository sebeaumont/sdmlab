
#include <iostream>
#include <utility>
#include <functional>

#include <boost/interprocess/allocators/allocator.hpp>
#include <boost/interprocess/managed_mapped_file.hpp>

#include <boost/interprocess/containers/string.hpp>

#include <boost/multi_index_container.hpp>
#include <boost/multi_index/ordered_index.hpp>
#include <boost/multi_index/hashed_index.hpp>
#include <boost/multi_index/member.hpp>


namespace gecko {

  namespace symtab {

    class table {
  
      //typedef int key_t;
      typedef boost::interprocess::basic_string<char> key_t;
      typedef int pay_t;

  
      typedef typename std::pair<key_t, pay_t> val_t;

      typedef boost::interprocess::managed_mapped_file segment_t;
      typedef segment_t::segment_manager segment_manager_t;

      typedef boost::interprocess::allocator<val_t, segment_manager_t> allocator_t;

      // tags
      struct from {};
      struct to {};

    public:
      
      typedef boost::multi_index_container<
        val_t,
        boost::multi_index::indexed_by<
          boost::multi_index::hashed_unique<
            boost::multi_index::tag<from>,
            boost::multi_index::member<val_t, key_t, &val_t::first>>,
          boost::multi_index::hashed_unique<
            boost::multi_index::tag<to>,
            boost::multi_index::member<val_t, pay_t, &val_t::second>>>,
        allocator_t> map_t;


      
    public:
      
      table(const std::string& name, const size_t size) {

        segment_t segment(boost::interprocess::open_or_create, name, size);
        allocator_t allocator(segment.get_segment_manager());

        map = segment.find_or_construct<map_t>(name)(allocator);
      }

      void insert(map_t::value_type& v) {
        map->insert(v);
      }

    private:
      map_t* map;
    };
    
  }
}
