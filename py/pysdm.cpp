// Copyright (c) 2016 Simon Beaumont - All Rights Reserved.
// boost python interface to sdm database

#include <boost/python.hpp>
#include <boost/mpl/if.hpp>
#include <boost/optional.hpp>
#include <boost/python.hpp>
#include <boost/type_traits/integral_constant.hpp>
#include <boost/utility/in_place_factory.hpp>

#include <rtl/database.hpp>

// testing... testing
boost::optional<bool> test_true() { return true; }
boost::optional<bool> test_false() { return false; }
boost::optional<bool> test_none() { return boost::none; }
boost::optional<bool> test_if(int n) {
  if (n==0)return false;
  else if (n==1) return true;
  else return boost::none;
}


namespace detail {

  // a typetrait that determines if the provided type is a boost::optional.
  template <typename T>
  struct is_optional : boost::false_type {};

  template <typename T>
  struct is_optional<boost::optional<T> > : boost::true_type {};

  // type used to provide meaningful compiler errors.

  template <typename>
  struct return_optional_requires_a_optional_return_type {};

  
  // ResultConverter converts a boost::optional object to Python None
  // if the object is empty (i.e. boost::none) or defers to
  // Boost.Python to convert object to a Python object
  
  template <typename T>
  struct to_python_optional {
    
    // only supports converting Boost.Optional types.  checked at
    // runtime.
    bool convertible() const { return detail::is_optional<T>::value; }

    // convert boost::optional object to Python None or a Boost.Python
    // object.
    PyObject* operator()(const T& obj) const {
      namespace python = boost::python;
      python::object result =
        obj                      // If boost::optional has a value, then
        ? python::object(*obj) // defer to Boost.Python converter.
        : python::object();    // Otherwise, return Python None.

      // python::object contains a handle which functions as
      // smart-pointer to the underlying PyObject.  As it will go out
      // of scope, explicitly increment the PyObject's reference
      // count, as the caller expects a non-borrowed (i.e. owned)
      // reference.
      return python::incref(result.ptr());
    }
    // documentation.
    const PyTypeObject* get_pytype() const { return 0; }
  };

} // namespace detail

// Converts a boost::optional to Python None if the object is equal to
// boost::none.  Otherwise, defers to the registered type converter to
// returs a Boost.Python object.

struct return_optional {
  template <class T> struct apply {
    
    // The to_python_optional ResultConverter only checks if T is
    // convertible at runtime.  However, the following MPL branch
    // cause a compile time error if T is not a boost::optional by
    // providing a type that is not a ResultConverter model.

    typedef typename boost::mpl::if_<
      detail::is_optional<T>,
      detail::to_python_optional<T>,
      detail::return_optional_requires_a_optional_return_type<T>
      >::type type;
  };
};


BOOST_PYTHON_MODULE(pysdm) {
  
  using namespace boost::python;
  using namespace molemind::sdm;

  // testing i/f
  def("true", test_true, return_value_policy<return_optional>());
  def("false", test_false, return_value_policy<return_optional>());
  def("none", test_none, return_value_policy<return_optional>());
  def("iff", test_if, return_value_policy<return_optional>());
    
  // python interface to c++ database class
  class_<database, boost::noncopyable>("database", init<std::size_t, std::size_t, std::string>())
    .def("free", &database::free_heap, "free heap memory")
    .def("heap", &database::heap_size, "size of heap")
    .def("add_symbol", &database::add_symbol, return_value_policy<return_optional>(), args("space", "name"),
         "add a symbol to a space")
    .def("search_symbols", &database::search_symbols, args("space", "prefix"),
         "return iterator of symbols with given prefix")
    .def("cardinality", &database::get_space_cardinality, return_value_policy<return_optional>())
    .def("iff", &database::test_if, return_value_policy<return_optional>())
    ;

  
}
