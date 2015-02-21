#include <iostream>
#include <boost/interprocess/allocators/allocator.hpp>
#include <boost/interprocess/managed_mapped_file.hpp>

#include "symbolic_vector.hpp"

namespace bip = boost::interprocess;
namespace gs = gecko::vspace;

// test refactoring of templates
int main(int argc, char** argv) {

  typedef bip::managed_mapped_file segment_t;
  typedef segment_t::segment_manager segment_manager_t;
  typedef bip::allocator<void, segment_manager_t> void_allocator_t;

  // segment
  segment_t segment(bip::open_or_create, "test.dat", 1024*1024*10);

  // symbol types
  typedef gs::symbol<segment_manager_t> symbol_t;
  typedef gs::elemental_vector<unsigned long, 32, segment_manager_t> e_vector_t;
  typedef gs::semantic_vector<unsigned long, 512, 32, segment_manager_t> s_vector_t;
  typedef gs::vector<unsigned long, 512, 32, segment_manager_t> vector_t;

  // try constructing symbolic vector flavours
  symbol_t   t("gloop", segment.get_segment_manager());
  e_vector_t e("foo", segment.get_segment_manager());
  s_vector_t s("bar", segment.get_segment_manager());
  // XXX multiple inheritance might be problematic... check this in context of table 
  vector_t v("floop", segment.get_segment_manager());

  // polymorphism
  std::string name("bloop");
  // todo: vector_t v1(name, segment.get_segment_manager());
  
  // test some operations
  s.superpose(e);
  s.superpose(v);
  v.superpose(e);
  v.superpose(v);
  
  //
  std::cout << t << "=" << sizeof(symbol_t) << std::endl
            << e << "=" << sizeof(e_vector_t) << std::endl
            << s << "=" << sizeof(s_vector_t) << std::endl
            << v << "=" << sizeof(vector_t) << std::endl;
  
  return 1;
}
