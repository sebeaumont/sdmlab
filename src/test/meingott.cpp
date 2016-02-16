#include <boost/interprocess/managed_mapped_file.hpp>
#include <boost/optional.hpp>
#include <map>

#include "../vspace/feature_space.hpp"

using namespace gecko::vspace;
typedef bip::managed_mapped_file segment_t;
  
// this is vspace implementation (fully featured! :)
typedef feature_space<unsigned long, 256, 16, segment_t> space;
