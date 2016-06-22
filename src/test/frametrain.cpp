/***************************************************************************
 * frametrain - simple online training via. stdin
 *
 * Copyright (c) Simon Beaumont 2012-2014 - All Rights Reserved.
 * See: LICENSE for conditions under which this software is published.
 ***************************************************************************/

#include <iostream>
#include <fstream>
#include <boost/algorithm/string.hpp>
#include <boost/date_time/posix_time/ptime.hpp>
#include <boost/date_time/microsec_time_clock.hpp>
#include <boost/tokenizer.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/program_options.hpp>
//#include <boost/filesystem.hpp>
#include <boost/optional.hpp>
#include <boost/optional/optional_io.hpp>

// we are really testing this
#include "../rtl/database.hpp"

using namespace std;

// wall clock timer

class timer {
  
public:
  
  timer(const string& name) : name(name), start(clock_t::local_time()) {}
  
  const size_t get_elapsed_micros() {
    
    time_t now(clock_t::local_time());
    elapsed_t d = now - start;
    return d.total_microseconds();
  }
  
  friend ostream& operator<<(ostream& os, timer& t) {
    os << "[" << t.get_elapsed_micros() << "] ";
    return os;
  }
  
private:
  
  typedef boost::posix_time::ptime time_t;
  typedef boost::date_time::microsec_clock<time_t> clock_t;
  typedef boost::posix_time::time_duration elapsed_t;
  
  string name;
  time_t start;
};


// tokenzier for tsv input line

void tokenize_line(const string& s, vector<string>& o) {
  boost::char_separator<char> sep("\t");
  boost::tokenizer<boost::char_separator<char>> tokens(s, sep);

  for(auto j = tokens.begin(); j != tokens.end(); ++j) {
    string t(*j);
    boost::trim(t);
    o.push_back(t);
  }
}

#define B2MB(b_) ((double)(b_)/(1024*1024))


////////////////////////////////
// entry point and command line

int main(int argc, const char** argv) {
  
  
  namespace po = boost::program_options;
  using namespace molemind::sdm;
  
  //
  string banner = "SDM frametrainer - copyright (c) 2016 simon beaumont - all rights reserved";
  
  // command line options
  //bool reflexive = true; // TODO
  bool symmetric = true; // TODO
  
  size_t initial_size;
  size_t maximum_size;
  
  po::options_description desc("Allowed options");
  po::positional_options_description p;
  p.add("image", -1);
  
  desc.add_options()
  ("help", "SDM frametrainer")
  ("space", po::value<string>(),
   "spacename")
  ("size", po::value<size_t>(&initial_size)->default_value(700),
   "initial size of heap in Mbytes")
  ("maxsize", po::value<size_t>(&maximum_size)->default_value(700),
   "maximum size of heap in Mbytes")
  ("image", po::value<string>(),
   "heap image name (should be a valid path)");
  
  po::variables_map opts;
  po::store(po::command_line_parser(argc, argv).options(desc).positional(p).run(), opts);
  po::notify(opts);
  
  if (opts.count("help")) {
    cout << desc <<  endl;
    return 1;
  }
  
  if (!opts.count("image") || !opts.count("space")) {
    cout << "heap image name and space are required parameters!" << endl;
    cout << desc <<  endl;
    return 3;
  }

  string heapfile(opts["image"].as<string>());
  string spacename(opts["space"].as<string>());

  cout << banner << endl;
  cout << "database:   " << heapfile << endl;
  cout << "trainspace: " << spacename << endl;
  cout << "size:       " << initial_size << endl;
  cout << "maxsize:    " << maximum_size << endl;
  

  // create database with requirement
  database db(initial_size * 1024 * 1024, maximum_size * 1024 * 1024, heapfile);
  
  // print out all the existing spaces and cardinalities
  vector<string> spaces = db.get_named_spaces();
  
  cout << "found spaces:" << endl;
  
  for (unsigned i = 0; i < spaces.size(); ++i) {
    cout << "\t" << spaces[i];
    // now actually get the pointers and cards
    auto spp = db.get_space_by_name(spaces[i]);
    cout << "[" << spp->entries() << "]" << endl;
  }
  
  cout << "read from stdin..." << endl;
  
  int docno = 1;
  // main i/o loop
  
  string input;
  
  // simple cline processor
  while (getline(cin, input)) {
    
    boost::trim(input);
    vector<string> tv;
    
    tokenize_line(input, tv);
    
    if (tv.size() == 0) {
      // assume we have a blank line which signals end of doc
      docno++;
      
    } else {
      // we need to permute the pairs treating this as a partial order with the relation > (which can be mapped to shift)
      // 1. create ordered set of terms in frame
      set<string> termset(tv.begin(), tv.end());
      
      // record number of pairs i.e. binomial coefficient which in
      // this case will be (choose 2 from termset.size)
      int n = 0;
      
      // pairwise combinatations of terms
      for (auto first = termset.begin(); first != termset.end(); ++first) {
        for (auto next = std::next(first); next != termset.end(); ++next) {
          // first R next
          db.superpose(spacename, *first, spacename, *next);
          // reify inverse?
          if (symmetric) db.superpose(spacename, *next, spacename, *first);
        }
      }
    }
  }
  
  // goodbye from me and goodbye from him...
  cout << endl
            << "end "
            << (db.check_heap_sanity() ? ":-)" : ":-(")
            << " size: " << B2MB(db.heap_size())
            << " free: " << B2MB(db.free_heap())
            << endl;
  
  return 0;
}

