// unit tests for runtime library
// copyright (c) 2015 Simon Beaumont. All Rights Reserved.

#include <cstdio>
#include <boost/algorithm/string.hpp>

#define BOOST_TEST_MODULE runtime_library
#include <boost/test/included/unit_test.hpp>

#include "database.hpp"

using namespace molemind::sdm;

// sizing
const std::size_t ini_size = 700 * 1024 * 1024;
const std::size_t max_size = 700 * 1024 * 1024;
const std::string image = "testheap.img";
const std::string test_lexicon = "/usr/share/dict/words";
const std::string test_space1 = "words";


// create and destroy rts
struct database_setup {
  database rts;
  database_setup () : rts(ini_size, max_size, image) {}
  ~database_setup () {
    // delete heapimage
    remove(image.c_str());
  }
};

//BOOST_AUTO_TEST_SUITE(database_library)

BOOST_FIXTURE_TEST_SUITE(database_library, database_setup)

BOOST_AUTO_TEST_CASE(rts_init)
{
  BOOST_CHECK_EQUAL(rts.heap_size(), ini_size);
  BOOST_REQUIRE_LT(rts.free_heap(), rts.heap_size());
  BOOST_REQUIRE(rts.check_heap_sanity());
}

BOOST_AUTO_TEST_CASE(rts_load_vectors) {
  std::ifstream ins(test_lexicon);
  BOOST_REQUIRE(ins.good());
  
  std::string fline;
  int loaded = 0;
  
  while(std::getline(ins, fline)) {
    boost::trim(fline);
    rts.add_symbol(test_space1, fline);
    loaded++;
  }
  

  auto card = rts.get_space_cardinality(test_space1);
  if (card) BOOST_CHECK_EQUAL(*card, loaded);
  
  BOOST_TEST_MESSAGE("search for: " << loaded);
  
  // lookup all vectors
  auto sit = rts.search_symbols(test_space1, "");
  for (auto it = sit.first; it != sit.second; ++it) {
    //std::cout << *it << std::endl;
    loaded--;
  }

  BOOST_CHECK_EQUAL(loaded, 0);
    
}

BOOST_AUTO_TEST_SUITE_END()
