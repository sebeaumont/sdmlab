//
//  main.cpp
//  gecko symbol table test harness
//
//  Created by Simon Beaumont on 07/12/2014.
//  Copyright (c) 2014 Simon Beaumont. All rights reserved.
//

#include <boost/interprocess/managed_shared_memory.hpp>
#include <boost/interprocess/allocators/allocator.hpp>
#include <boost/interprocess/managed_mapped_file.hpp>
#include <boost/interprocess/containers/string.hpp>
#include <boost/interprocess/containers/map.hpp>

#include <iostream>


int symbol_table(size_t size) {

  //using namespace boost;
  using namespace boost::interprocess;
  typedef managed_shared_memory::allocator<char>::type char_allocator;
  typedef basic_string<char, std::char_traits<char>, char_allocator> shm_string;

  /*
  struct symbol {
    size_t id;
    shm_string name;

    symbol(size_t id_, const char* name_, const char_allocator& a) : id(id_), name(name_, a) {}
  };
  */

  
  wmanaged_mapped_file mfile(open_or_create, "test0", size); // eh?
  return(1);
}


int main(int argc, const char * argv[]) {
  std::cout << "Table loader test!\n";

  int x = symbol_table(1024*1024*10);
  
  return 0;
}
