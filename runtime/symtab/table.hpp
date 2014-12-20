/***************************************************************************
 * table.hpp - symbol table (part of the Gecko qdsm environment)
 *
 * Copyright (c) Simon Beaumont 2012-2014 - All Rights Reserved.
 * See: LICENSE for conditions under which this software is published.
 ***************************************************************************/

#include <iostream>

#include <boost/interprocess/allocators/allocator.hpp>
#include <boost/interprocess/managed_mapped_file.hpp>
#include <boost/interprocess/containers/string.hpp>

#include <boost/multi_index_container.hpp>
#include <boost/multi_index/ordered_index.hpp>
#include <boost/multi_index/hashed_index.hpp>
#include <boost/multi_index/member.hpp>
#include <boost/optional.hpp>


using boost::multi_index_container;
using namespace boost::multi_index;
namespace bip = boost::interprocess;

namespace gecko {

  namespace symtab {

    // memory mapped file based symbol table
    
    class table final {

      // strings allocated in the segment
      
      typedef bip::basic_string<
        char,
        std::char_traits<char>,
        bip::allocator<char, bip::managed_mapped_file::segment_manager>> shared_string_t;
      
      typedef std::size_t id_t;

      enum status_t {NEW, USED, OLD, FREE};

      // symbols stored in the shared multi_index container
      
      struct symbol final {
        
        shared_string_t name;
        id_t id;
        status_t flags;

        // constructor requires a shared string
        symbol(const shared_string_t& s, const id_t& i) : name(s), id(i), flags(NEW) {}

        friend std::ostream& operator<<(std::ostream& os, const symbol& s) {
          os << "(" << s.name << ", " << s.id << ", " << s.flags << ")";
          return os;
        }
      };

      // managed segments and allocator types
      
      typedef bip::managed_mapped_file                  segment_t;
      typedef segment_t::segment_manager                segment_manager_t;
      typedef bip::allocator<symbol, segment_manager_t> allocator_t;
      
      // shared memory mapped multi index container type with it's indexes
      
      typedef multi_index_container<
        symbol,
        indexed_by<
          hashed_unique<BOOST_MULTI_INDEX_MEMBER(symbol, shared_string_t, name)>,
          hashed_unique<BOOST_MULTI_INDEX_MEMBER(symbol, id_t, id)>
          >, allocator_t
        > symbol_table_t;

      // shared string helpers
      
      inline shared_string_t shared_string(const std::string& s) {
        return shared_string_t(s.c_str(), shared_string_t::allocator_type(segment.get_segment_manager()));  
      }

      inline shared_string_t shared_string(const char* s) {
        return shared_string_t(s, shared_string_t::allocator_type(segment.get_segment_manager()));  
      }

      
    public:
      
      table(const std::string& s, const size_t& size)
        : name(s.c_str()),
          segment(boost::interprocess::open_or_create, name, size),
          allocator(segment.get_segment_manager()) {
        // ensure multi_index container is constructed
        db = segment.find_or_construct<symbol_table_t>(name)(allocator);
      }


      ~table() {
        // should we remove the shared_memory_object (by name) here as well?
        segment.flush();
      }

      // delete the rest of the gang

      table(const table&) = delete;
      table(table&&) = delete;
      const table& operator=(const table&) = delete;
      const table& operator=(table&&) = delete;

      // printer

      friend std::ostream& operator<<(std::ostream& os, table& t) {
        os << "(" << (t.check_sanity() ? ":-) " : ":-( ") << t.tablename()
           << "[" << t.entries() << "] " << t.get_free() << "/" << t.size() << ")";
        return os;
      }  

      // insertion
      
      inline void insert(const std::string& k, const id_t& i) {
        db->insert(symbol(shared_string(k), i));
      }

      
      // xxx not sure how expensive these optional values are at runtime

      // lookup by name
      
      inline boost::optional<const symbol&> get_symbol(const char* k) {
        typedef symbol_table_t::nth_index<0>::type symbol_by_name;
        
        symbol_by_name& name_idx = db->get<0>();
        symbol_by_name::iterator i = name_idx.find(shared_string(k));
        if (i == name_idx.end()) return boost::none;
        else return *i;
      }

      // lookup by name

      inline boost::optional<const symbol&> get_symbol(const std::string& k) {
        typedef symbol_table_t::nth_index<0>::type symbol_by_name;
        
        symbol_by_name& name_idx = db->get<0>();
        symbol_by_name::iterator i = name_idx.find(shared_string(k));
        if (i == name_idx.end()) return boost::none;
        else return *i;
      }

      // lookup by index

      inline boost::optional<const symbol&> get_symbol(const id_t& k) {
        typedef symbol_table_t::nth_index<1>::type symbol_by_id;
        
        symbol_by_id& id_idx = db->get<1>(); 
        symbol_by_id::iterator i = id_idx.find(k);
        if (i == id_idx.end()) return boost::none;
        else return *i;
      }
      
      // delegated iterators

      typedef symbol_table_t::iterator iterator;
      typedef symbol_table_t::const_iterator const_iterator;
      
      inline iterator begin() { return db->begin(); }
      inline iterator end() { return db->end(); }

      // delegated properties
      
      inline const size_t size() { return segment.get_size(); }
      inline const size_t get_free() { return segment.get_free_memory(); }
      inline const bool check_sanity() { return segment.check_sanity(); }
      inline const size_t entries() { return db->size(); }
      inline const char* tablename() const { return name; }
      // TODO: shrink_to_fit, grow
      
    private:    
      const char*     name; 
      symbol_table_t* db;
      segment_t       segment;
      allocator_t     allocator;
    };
    
  }
}
