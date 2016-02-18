//
//  runtime_exceptions.hpp
//
//  Copyright (c) 2012-2014 Simon Beaumont All rights reserved.

#pragma once

namespace sdm {

  /* exception class for library */
  class error : public std::runtime_error {
  public:
    error(const std::string& what_arg) : std::runtime_error(what_arg) {}
    error(const char* what_arg) : std::runtime_error(what_arg) {}
  };
  
  
  class not_found : public std::runtime_error {
  public:
    not_found(const std::string& what_arg) : std::runtime_error(what_arg) {}
    not_found(const char* what_arg) : std::runtime_error(what_arg) {}
  };
  
  class vector_not_found : public not_found {
  public:
    vector_not_found(const std::string& what_arg) : not_found(what_arg) {}
    vector_not_found(const char* what_arg) : not_found(what_arg) {}
  };
  
  class space_not_found : public not_found {
  public:
    space_not_found(const std::string& what_arg) : not_found(what_arg) {}
    space_not_found(const char* what_arg) : not_found(what_arg) {}
  };
}

