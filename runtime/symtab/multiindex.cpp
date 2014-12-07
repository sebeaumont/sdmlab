
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

struct from{};
struct to{};


int main(int argc, char** argv) {

  
  
  //typedef int key_t;
  typedef boost::interprocess::basic_string<char> key_t;
  typedef int pay_t;

  
  typedef typename std::pair<key_t, pay_t> val_t;

  typedef boost::interprocess::managed_mapped_file                  segment_t;
  typedef segment_t::segment_manager                                segment_manager_t;

  typedef boost::interprocess::allocator< val_t, segment_manager_t> allocator_t;
  
  
  typedef boost::multi_index_container<
    val_t,
    boost::multi_index::indexed_by<
      boost::multi_index::hashed_unique<
        boost::multi_index::tag<from>,
        boost::multi_index::member<val_t, key_t, &val_t::first>
      >,
      boost::multi_index::hashed_unique<
        boost::multi_index::tag<to>,
        boost::multi_index::member<val_t, pay_t, &val_t::second>
      >
    >,
    allocator_t
  > map_t;

  {
    std::cout << "Writing" << std::endl;
    segment_t segment(boost::interprocess::open_or_create,"scratch.multiindex",10000);
    allocator_t allocator(segment.get_segment_manager());

    map_t* map = segment.find_or_construct<map_t>("multiindex")(allocator);

    std::cout << "There were " << map->size() << " relations" << std::endl;
    for( map_t::const_iterator iter = map->begin(); iter != map->end(); ++iter ) {
      std::cout << iter->first << " <--> " << iter->second << std::endl;
    }

    map->insert( map_t::value_type("foo", 20 ) );
    map->insert( map_t::value_type("bar", 42 ) );

    std::cout << "There are " << map->size() << " relations" << std::endl;
    for( map_t::const_iterator iter = map->begin(), iend = map->end(); 
            iter != iend; ++iter ) {
      std::cout << iter->first << " <--> " << iter->second << std::endl;
    }
    
  } 

  {
    std::cout << "Reading" << std::endl;
    segment_t*   segment   = new segment_t(boost::interprocess::open_read_only,"scratch.multiindex");
    allocator_t* allocator = new allocator_t(segment->get_segment_manager());

    map_t* map = segment->find<map_t>("multiindex").first;
    
    std::cout << "There were " << map->size() << " relations" << std::endl;
    for( map_t::const_iterator iter = map->begin(); iter != map->end(); ++iter ) {
      std::cout << iter->first << " <--> " << iter->second << std::endl;
    }
    
    delete allocator;
    delete segment;
  }

  // segment_t::shrink_to_fit("scratch.multiindex");
  return 0;
}
