/* Copyright (C) Simon Beaumont 2015-2016 - All Rights Reserved */
#pragma once

#include <boost/interprocess/containers/string.hpp>
#include <boost/interprocess/containers/vector.hpp>
#include <boost/multi_index_container.hpp>
#include <boost/multi_index/ordered_index.hpp>
#include <boost/multi_index/hashed_index.hpp>
#include <boost/multi_index/random_access_index.hpp>
#include <boost/multi_index/member.hpp>
#include <boost/optional.hpp>

#include "elemental_vector.hpp"
//#include "vector_space.hpp"

namespace sdm {

  namespace mms {

    using boost::multi_index_container;
    using namespace boost::multi_index;
    namespace bip = boost::interprocess;
    
    /* 
     * this class template can be instantiated in runtime library
     * source or inlined in application code; the important
     * implementation details are the types and sizes of the vectors
     * of the underlying vector space and the sparsity of the random
     * (a.k.a. elemental) vectors
     */

    template <typename VectorElementType, std::size_t VArraySize, std::size_t ElementalBits, class SegmentClass>

    /*
     * for a symbol_space
     */
    
    class symbol_space {
      
      typedef SegmentClass segment_t;
      
      // Heap allocators derived from segment
      
      typedef typename segment_t::segment_manager segment_manager_t;
      typedef bip::basic_string<char,std::char_traits<char>, bip::allocator<char, segment_manager_t>> shared_string_t;
      typedef typename bip::allocator<void, segment_manager_t> void_allocator_t;

      // SDM vector types
      //typedef VectorElementType vector_t[VArraySize];
      typedef elemental_vector<VectorElementType, segment_manager_t> elemental_vector_t;


      
      // Symbolic vector -- indexed by: name hash, r&b tree for prefix of name, random access 

      struct symbol {

        // symbol_t state
        shared_string_t name;
        elemental_vector_t super;        // 50-50 balanced elementals 
        elemental_vector_t suber;

        // constructor
        symbol(const char* s, const void_allocator_t& void_alloc)
          : name(s, void_alloc), super(ElementalBits/2, 0, void_alloc), suber(ElementalBits/2, 0, void_alloc) {}
      
        // printer
        friend std::ostream& operator<<(std::ostream& os, const symbol& s) {
          os << s.name << " (" << sizeof(symbol) << "," << ElementalBits <<  ")";
          return os;
        }
      };

      // allocator for symbol
      
      typedef bip::allocator<symbol, segment_manager_t> symbol_allocator_t;
      
      // shared string helpers
      
      inline shared_string_t shared_string(const std::string& s) {
        return shared_string_t(s.c_str(), allocator);
      }

      inline shared_string_t shared_string(const char* s) {
        return shared_string_t(s, allocator); 
      }

      // partial (prefix) string comparison
      
      struct partial_string {
        partial_string(const shared_string_t& str) : str(str) {}
        shared_string_t str;
      };
      
      struct partial_string_comparator {
        bool operator()(const shared_string_t& x, const shared_string_t& y) const {
          return x<y;
        }

        bool operator()(const shared_string_t& x,const partial_string& y) const {
          return x.substr(0,y.str.size())<y.str;
        }

        bool operator()(const partial_string& x,const shared_string_t& y) const {
          return x.str<y.substr(0,x.str.size());
        }
      };


      // shared memory mapped multi index container type with it's indexes
      
      typedef multi_index_container<
        symbol,
        indexed_by<
          hashed_unique<BOOST_MULTI_INDEX_MEMBER(symbol, shared_string_t, name)>,
          ordered_unique<BOOST_MULTI_INDEX_MEMBER(symbol, shared_string_t, name), partial_string_comparator>,
          random_access<>
          >, symbol_allocator_t
        > symbol_table_t;


      // vector_space implemented as a vector of vectors
      
      typedef VectorElementType element_t;

      typedef bip::allocator<element_t, segment_manager_t> element_allocator_t;
      
      typedef bip::vector<element_t, element_allocator_t> vector_t;
      typedef bip::allocator<vector_t, segment_manager_t> vector_allocator_t;
      
      typedef bip::vector<vector_t, vector_allocator_t> vector_vector_t;

      struct vector_space : public vector_vector_t {

        vector_space(const void_allocator_t& a) : vector_vector_t(a) {}

        // c'tor with reservation size
        vector_space(const::size_t n, const void_allocator_t& a) : vector_vector_t(a) {
          this->reserve(n);
        }
      };

      typedef vector_space vector_space_t;

    public:

      // constructor pre-allocate cache space
            
      symbol_space(const std::string& s, const::size_t n, segment_t& m)
        : name(s), segment(m), allocator(segment.get_segment_manager()) {
        // ensure multi_index container is constructed: this is the symbol space
        index = segment.template find_or_construct<symbol_table_t>(name.c_str())(allocator);
        std::string vs_name = "__" + name;
        vectors = segment.template find_or_construct<vector_space_t>(vs_name.c_str())(n, allocator);
      }

      // default cache space
      
      symbol_space(const std::string& s, segment_t& m)
        : name(s), segment(m), allocator(segment.get_segment_manager()) {
        // ensure multi_index container is constructed: this is the symbol space
        index = segment.template find_or_construct<symbol_table_t>(name.c_str())(allocator);
        std::string vs_name = "__" + name;
        vectors = segment.template find_or_construct<vector_space_t>(vs_name.c_str())(allocator);
      }

      
      // destructor
      
      ~symbol_space() {
        // should we remove the shared_memory_object (by name) here as well?
        // segment is global so flushing should be manged by owner... 
        segment.flush();
      }

      // delete the rest of the gang don't ever want to copy a space -- but move?

      symbol_space(const symbol_space&) = delete;
      symbol_space(symbol_space&&) = delete;
      const symbol_space& operator=(const symbol_space&) = delete;
      const symbol_space& operator=(symbol_space&&) = delete;

      
      // printer symbol_space stats
      
      friend std::ostream& operator<<(std::ostream& os, symbol_space& t) {
        os << t.spacename() << " #" << t.entries(); 
        return os;
      }  
     

      typedef symbol symbol; // public face of symbol
      typedef vector_t vector; //xx
      
      
      // insert
      
      boost::optional<const std::size_t> insert(const std::string& k) {
        auto p = index->insert(symbol(k.c_str(), allocator));
        if (p.second) {
          // inserted string iterator maps to index
          //xvectors->push_new();
          return n2i(p.first);
          //return (*vectors)[n2i(p.first)];
        } 
        else return boost::none;
      }
      
      // random access
      
      typedef typename symbol_table_t::template nth_index<2>::type symbol_by_index;
      
      // overload [] and delegate
      
      inline const symbol& operator[](std::size_t i) {
        symbol_by_index& symbols = index->template get<2>(); 
        return symbols[i]; 
      }
      
      // lookup by name
      
      typedef typename symbol_table_t::template nth_index<0>::type symbol_by_name;

      inline boost::optional<const symbol&> get(const std::string& k) {
        symbol_by_name& name_idx = index->template get<0>();
        typename symbol_by_name::iterator i = name_idx.find(shared_string(k));
        if (i == name_idx.end()) return boost::none;
        else return *i;
      }

      // xxx under construction: return vectors
      
      inline boost::optional<vector&> get_vector(const std::string& k) {
        symbol_by_name& name_idx = index->template get<0>();
        typename symbol_by_name::iterator it = name_idx.find(shared_string(k));
        if (it == name_idx.end()) return boost::none;
        else return (*vectors)[n2i(it)];
      }
      
      
      // search by prefix
      
      typedef typename symbol_table_t::template nth_index<1>::type symbol_by_prefix;  
      typedef typename symbol_by_prefix::iterator symbol_iterator;

      inline std::pair<symbol_iterator, symbol_iterator> search(const std::string& k) {
        symbol_by_prefix& name_idx = index->template get<1>();
        return name_idx.equal_range(partial_string(shared_string(k)));
      }

      // project the iterator into direct index
      
      inline const std::size_t n2i(typename symbol_by_name::iterator& nit) {
        return ((index->template project<2>(nit)) - (index->template get<2>().begin()));
      }
      
      // delegated space iterators

      typedef typename symbol_table_t::iterator iterator;
      typedef typename symbol_table_t::const_iterator const_iterator;

      //inline iterator begin() { return index->begin(); }
      //inline iterator end() { return index->end(); }

      inline const_iterator begin() { return index->begin(); }
      inline const_iterator end() { return index->end(); }

      // delegated properties
      inline const size_t entries() { return index->size(); }
      inline const std::string spacename() const { return name; }

      // TODO: shrink_to_fit, grow
      
    private:    

      std::string          name; 
      symbol_table_t*      index;
      vector_space_t*      vectors; 
      segment_t&           segment;
      void_allocator_t     allocator;
  
    };
  }
}
