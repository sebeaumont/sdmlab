#pragma once
/**
 * This is the parser for our language
 **/
#include <boost/spirit/include/qi.hpp>

#include <iostream>

namespace molemind {
  namespace sdm {
    namespace vml {

      namespace qi = boost::spirit::qi;
      

      // A function object
      struct print_action {
        void operator()(int const& i, qi::unused_type, qi::unused_type) const {
          std::cout << "Int#"<< i << std::endl;
        }
      };

      void test_parse(void) {
        char const *first = "{43}", *last = first + std::strlen(first);
        qi::parse(first, last, '{' >> qi::int_[print_action()] >> '}');

      }
    }
  }
}
