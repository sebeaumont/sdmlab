//
//  runtim_exceptions.hpp
//
//  Copyright (c) 2012-2014 Simon Beaumont All rights reserved.


#ifndef __RUNTIME_EXCEPTIONS_HPP__
#define __RUNTIME_EXCEPTIONS_HPP__


namespace dsm {

  /* exception class for library */
  class error : public std::runtime_error {

  public:
    error(const std::string& what_arg) : std::runtime_error(what_arg) {}
    error(const char* what_arg) : std::runtime_error(what_arg) {}
  };

  
}


#endif
