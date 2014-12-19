/***************************************************************************
 * main.cpp symbol table test utility - part of the Gecko qdsm environment.
 *
 * Copyright (c) Simon Beaumont 2012-2014 - All Rights Reserved.
 * See: LICENSE for conditions under which this software is published.
 ***************************************************************************/

#include <iostream>
#include <boost/algorithm/string.hpp>
#include <boost/date_time/posix_time/ptime.hpp>
#include <boost/date_time/microsec_time_clock.hpp>
#include <boost/tokenizer.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/program_options.hpp>
#include <boost/filesystem.hpp>
#include <boost/optional.hpp>
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

////////////////////////////////
// entry point and command line

int main(int argc, const char** argv) {
  
  namespace po = boost::program_options;
  using gecko::symtab::table;

  std::size_t requested_size;
  po::options_description desc("Allowed options");
  po::positional_options_description p;
  p.add("name", -1);

  desc.add_options()
    ("help", "Gecko qdsm symbol table test utility")
    ("size", po::value<std::size_t>(&requested_size)->default_value(0),
     "requested size for table in bytes")
    ("name", po::value<std::string>(),
     "name (can be a path)");

  po::variables_map opts;
  po::store(po::command_line_parser(argc, argv).options(desc).positional(p).run(), opts);
  po::notify(opts);
  
  if (opts.count("help")) {
    std::cout << desc <<  std::endl;
    return 1;
  } else if (!opts.count("name")) {
    std::cout << "table name is required!" << std::endl;
    return 3;
  }

  std::string tablename(opts["name"].as<std::string>());

  if (!boost::filesystem::exists(tablename) && requested_size == 0) {
    std::cout << "cannot create new table: " << tablename
              << " with: " << requested_size << " bytes" << std::endl;
    return 5;
  }
  
  // todo sizing and stuff...
  
  table mytable(tablename, requested_size);
  
  std::cout << mytable << std::endl;
  
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
      timer mytimer("command timer");
      
      // dispatch command
      if (boost::iequals(cv[0], "=")) {
        // this is bogus we would get the index from the varray allocator
        // N.B. insertion suceeds even if the symbol already exists in the table 
        mytable.insert(cv[1], lastindex++);
        // lookup the inserted symbol
        if (auto sym = mytable.get_symbol(cv[1]))
          std::cout << *sym << std::endl;
        else
          std::cout << cv[1] << ": not found!" << std::endl;
        
      } else if (boost::iequals(cv[0], "<")) {
        
        ; // load file
        std::cout << mytimer << cv[0] << std::endl;
        
      } else if (boost::iequals(cv[0], ">")) {
        ; // export file

      } else if (boost::iequals(cv[0], ".")) {
        std::cout << "juju" << std::endl;

      } else
        std::cout << "syntax error:" << input << std::endl;

      
    } else {
      // default to lookup if no args
      if (auto sym = mytable.get_symbol(cv[0]))
        std::cout << *sym << std::endl;
      else
        std::cout << cv[0] << ": not found!" << std::endl;
    }
    
    std::cout << prompt;  
  }
  
  // goodbye from me and goodbye from him...
  std::cout << std::endl << mytable << std::endl << "goodbye" << std::endl;
  
  return 0;
  
}

