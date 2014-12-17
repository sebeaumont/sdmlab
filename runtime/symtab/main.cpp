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
#include <boost/tokenizer.hpp>
#include <boost/lexical_cast.hpp>

#include "table.hpp"


// wall clock timer

class timer {
  
public:

  timer(const std::string& name) : name(name), start(clock_t::local_time()) {}

  const std::size_t get_elapsed_micros() {
    
    time_t now(clock_t::local_time());
    elapsed_t d = now - start;
    return d.total_microseconds();
  }

  friend std::ostream& operator<<(std::ostream& os, timer& t) {
    os << "[" << t.get_elapsed_micros() << "] ";
    return os;
  }

private:

  typedef boost::posix_time::ptime time_t;
  typedef boost::date_time::microsec_clock<time_t> clock_t;
  typedef boost::posix_time::time_duration elapsed_t;
  
  std::string name;
  time_t start;
};


// tokenzier for input line

void tokenize_command(const std::string& s, std::vector<std::string>& o) {
  typedef boost::escaped_list_separator<char> ls_t;
  typedef boost::tokenizer<ls_t> toz_t;
  
  ls_t els("\\"," \t","\"\'");
  toz_t tok(s, els);
  
  for(toz_t::iterator j = tok.begin(); j != tok.end(); ++j) {
    std::string t(*j);
    boost::trim(t);
    o.push_back(t);
  }
}


int main(int argc, const char** argv) {
  
  using gecko::symtab::table;

  // todo commad args for space name and size
  table mytable("test-words", 1024*1024*100);
  
  std::cout << "space:    " << std::endl;
  std::cout << "is sane:  " << mytable.check_sanity() << std::endl;
  std::cout << "size:     " << mytable.size() << std::endl;
  std::cout << "free:     " << mytable.get_free() << std::endl;
  std::cout << "entries:  " << mytable.entries() << std::endl;
  
  int lastindex = mytable.entries();
  
  std::string prompt("Ψ> ");
  std::string input;
  
  std::cout << prompt;  


  // simple command processor
  while (std::getline(std::cin, input)) {
    boost::trim(input);

    std::vector<std::string> cv;
    tokenize_command(input, cv);

    if (cv.size() > 1) {  
      // assume we have a command
      timer mytimer("elapsed");
      
      // dispatch command
      if (boost::iequals(cv[0], "="))
        mytable.insert(cv[1], lastindex++);
      
      else if (boost::iequals(cv[0], "<"))
        ; // load file
      else if (boost::iequals(cv[0], ">"))
        ; // export file
      else if (boost::iequals(cv[0], "."))
        std::cout << "juju" << std::endl;
      else
        std::cout << "syntax error:" << input << std::endl;
      
      std::cout << mytimer << cv[0] << std::endl;

      
    } else {
      // default to lookup if no args
      auto hit = mytable.get_symbol(cv[0]);
      std::cout << *hit << std::endl;
    }
    
    std::cout << prompt;
  }
  
  std::cout << std::endl << "goodbye" << std::endl;
  std::cout << "entries:  " << mytable.entries() << std::endl;

  return 0;
  
}

