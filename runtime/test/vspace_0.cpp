/***************************************************************************
 * basic paranoia test case of vspace
 *
 * Copyright (c) Simon Beaumont 2012-2014 - All Rights Reserved.
 * See: LICENSE for conditions under which this software is published.
 ***************************************************************************/
#include <cstdio>
#include <iostream>
#include <boost/algorithm/string.hpp>
#include <boost/interprocess/managed_mapped_file.hpp>


#define BOOST_TEST_MODULE vspace-0
#include <boost/test/included/unit_test.hpp>
#include "feature_space.hpp"

namespace gs = gecko::vspace;
namespace bip = boost::interprocess;
    
const std::size_t requested_size = 2 * 1024 * 1024; // bytes
const std::string tablename = "woobongaruru";
const std::string heapfile = "vpsace-0.img";
const std::string v0 = "vector-0";

typedef bip::managed_mapped_file segment_t;
typedef gs::feature_space<unsigned long, 256, 16, segment_t> space_t;

//int main(int argc, char** argv) {


// test context
struct test_setup {
  segment_t segment;
  space_t vspace;
  
  test_setup() : segment(bip::open_or_create, heapfile.c_str(), requested_size),
                 vspace(tablename, segment) {}
  
  ~test_setup() { remove(heapfile.c_str()); }
};

/*
  test_setup ctx;
  std::cout << ctx.vspace << std::endl; 
  ctx.vspace.insert(v0);
  std::cout << ctx.vspace << std::endl;

  return 0;
}
*/

BOOST_FIXTURE_TEST_SUITE(vspace_0, test_setup)

BOOST_AUTO_TEST_CASE(insert_vector) {
  std::cout << vspace << std::endl;
  //BOOST_TEST_MESSAGE(msg);
  vspace.insert(v0);
  std::cout << vspace << std::endl;
  //BOOST_TEST_MESSAGE(msg);
  // 5. retrieve vector
  BOOST_REQUIRE(vspace.get(v0));
}

BOOST_AUTO_TEST_SUITE_END()

  
