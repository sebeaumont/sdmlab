//
//  main.cpp
//  gecko symbol table test harness
//
//  Created by Simon Beaumont on 07/12/2014.
//  Copyright (c) 2014 Simon Beaumont. All rights reserved.
//

#include <iostream>
#include <boost/algorithm/string.hpp>
#include <boost/date_time/posix_time/ptime.hpp>
#include <boost/date_time/microsec_time_clock.hpp>
#include <boost/interprocess/managed_shared_memory.hpp>

#include "table.hpp"


// timing code
class timer {

public:

  timer(const std::string & name) : name(name), start(boost::date_time::microsec_clock<boost::posix_time::ptime>::local_time()) {}

  const std::size_t get_elapsed_micros() {
    using namespace std;
    using namespace boost;
    
    posix_time::ptime now(date_time::microsec_clock<posix_time::ptime>::local_time());
    posix_time::time_duration d = now - start;
    
    return d.total_microseconds();
  }
  
private:
  std::string name;
  boost::posix_time::ptime start;
};



int main(int argc, const char** argv) {
  
  using gecko::symtab::table;

  std::cout << "table loader test:" << std::endl;
  table mytable("test-words", 1024*1024*100);

  std::string line;
  std::size_t n = 0;

  timer mytimer("load");
  
  // read terms from stdin and insert into table
  while (std::getline(std::cin, line)) {
    boost::trim(line);
    //std::cout << line << "\t" << n << std::endl;
    mytable.insert(line, n);
    n++;
  }
  
  std::cout << "inserted: " << n << " records in: " << mytimer.get_elapsed_micros() << std::endl;
  

  for(table::const_iterator iter = mytable.begin(); iter != mytable.end(); ++iter ) {
    std::cout << *iter;
  }

   return 0;
}

