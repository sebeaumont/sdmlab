/***************************************************************************
 * topology.cpp - simple online query via. stdin
 *
 * Copyright (c) Simon Beaumont 2012-2016 - All Rights Reserved.
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

// runtime library

#include "../rtl/database.hpp"

// and timing

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
  
  // announce
  string banner = "SDM topology - Copyright (c) 2016 Simon Beaumont - All Rights Reserved. See LICENCE for terms and conditions.";
  
  // command line options
  size_t initial_size;
  size_t maximum_size;
  
  po::options_description desc("Allowed options");
  po::positional_options_description p;
  p.add("image", -1);
  
  desc.add_options()
  ("help", "SDM topology")
  ("size", po::value<size_t>(&initial_size)->default_value(700),
   "initial size of heap in MB")
  ("maxsize", po::value<size_t>(&maximum_size)->default_value(700),
   "maximum size of heap in MB")
  ("image", po::value<string>(),
   "database image file (should be a valid path)");
  
  po::variables_map opts;
  
  // XXX try...catch
  po::store(po::command_line_parser(argc, argv).options(desc).positional(p).run(), opts);
  po::notify(opts);
  
  if (opts.count("help")) {
    cerr << banner << endl;
    cerr << desc <<  endl;
    return 1;
  }
  
  if (!opts.count("image")) {
    cerr << "heap image name is required parameter!" << endl;
    cerr << desc <<  endl;
    return 3;
  }
  
  
  string heapfile(opts["image"].as<string>());
  
  cerr << banner << endl;
  cerr << "=============================================" << endl;
  cerr << "database:   " << heapfile << endl;
  cerr << "size:       " << initial_size << endl;
  cerr << "maxsize:    " << maximum_size << endl;
  cerr << "=============================================" << endl;
  
  // create database with requirement
  database db(initial_size * 1024 * 1024, maximum_size * 1024 * 1024, heapfile);
  
  // print out all the existing spaces and cardinalities
  vector<string> spaces = db.get_named_spaces();
  
  cerr << "existing spaces:" << endl;
  
  for (unsigned i = 0; i < spaces.size(); ++i) {
    cerr << "\t" << spaces[i];
    // now actually get the pointers and cards
    auto spp = db.get_space_by_name(spaces[i]);
    cerr << "[" << spp->entries() << "]" << endl;
  }
  
  cerr << "reading topology requests from stdin..." << endl;
  // accumualated stats...
  u_int requests = 0;

  // main i/o loop binds these
  string tgt_space;
  string src_space;
  string src_name;
  double p_lower;
  double r_upper;
  std::size_t max_card;
  
  // simpleline processor
  while (cin >> tgt_space >> src_space >> src_name >> p_lower >> r_upper >> max_card) {
    requests++;
    
    // toplogy of n nearest neighbours satisfying p, d constraints
    boost::optional<database::space::topology> t = db.neighbourhood(tgt_space, src_space, src_name,
                                                                    p_lower, r_upper, max_card);
    // printer
    if (t) for (auto p : *t) {
      cout << p << '\n';
    } else {
      cout << endl;
    }
    cout << endl;
  }
  
  
  // goodbye from me and goodbye from him...
  cerr << "[" << requests << "]" << endl;
  cerr << (db.check_heap_sanity() ? ":-)" : ":-(") << " free: " << B2MB(db.free_heap()) << endl;
  
  return 0;
}
