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
#include <boost/interprocess/containers/vector.hpp>
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

    typedef bip::managed_mapped_file segment_t;

    // memory mapped file based symbol table
    template <typename T, std::size_t N, std::size_t S> 
    class table final {
      
      // managed segments and allocator types

      typedef segment_t::segment_manager                       segment_manager_t;

      typedef bip::allocator<void, segment_manager_t>          void_allocator_t;
      
      // strings allocated in the segment
      typedef bip::basic_string<char, std::char_traits<char>, bip::allocator<char, segment_manager_t>> shared_string_t;
      
      // vectors of int allocated in the segment
      typedef bip::allocator<T, segment_manager_t>             bitv_allocator_t;
      typedef bip::vector<T, bitv_allocator_t>                 bitv_vector_t;
      typedef bip::allocator<bitv_vector_t, segment_manager_t> bitv_vector_allocator_t;

      // id and status 
      typedef std::size_t id_t;
      enum status_t {NEW, USED, OLD, FREE}; // TODO mainly for GC

      // symbols stored in the shared multi_index container

      // TODO feasibility of several types of symbol: semantic vectors, elemental vectors and joint
      // to ease storage
      
      struct symbol final {
        
        shared_string_t name;
        id_t id; // maybe redundant -- also index
        status_t flags;
        bitv_vector_t semv;
        bitv_vector_t elev;

        // construct a symbol in the table XXX TODO elev needs random entries
        
        symbol(const char* s, const id_t& i, const void_allocator_t& void_alloc)
          : name(s, void_alloc), id(i), flags(NEW), semv(N, 0, void_alloc), elev(S, 0, void_alloc) {}
        

        friend std::ostream& operator<<(std::ostream& os, const symbol& s) {
          os << "(" << s.name << ", " << s.id << ", " << s.flags << ", " << s.semv.size() << ", " << s.elev.size() << ")";
          return os;
        }
      };

      // symbol allocator
   
      typedef bip::allocator<symbol, segment_manager_t> symbol_allocator_t;
      
      // shared memory mapped multi index container type with it's indexes
      // TODO add non_unique name prefix btree/rb index
      
      typedef multi_index_container<
        symbol,
        indexed_by<
          hashed_unique<BOOST_MULTI_INDEX_MEMBER(symbol, shared_string_t, name)>,
          hashed_unique<BOOST_MULTI_INDEX_MEMBER(symbol, id_t, id)>
          >, symbol_allocator_t
        > symbol_table_t;

      
      // shared string helpers
      
      inline shared_string_t shared_string(const std::string& s) {
        return shared_string_t(s.c_str(), shared_string_t::allocator_type(allocator));  
      }

      inline shared_string_t shared_string(const char* s) {
        return shared_string_t(s, shared_string_t::allocator_type(allocator));  
      }

      
    public:
      
      // the segment is a global memory mapped file we need to share this across multiple namespaces
      // that is not enforced here but relies on caller doing so
      // TODO factory to do this...
            
      table(const std::string& s, segment_t& m)
        : name(s.c_str()), segment(m), allocator(segment.get_segment_manager()) {
        // ensure multi_index container is constructed: this is the symbol table
        db = segment.find_or_construct<symbol_table_t>(name)(allocator);
      }


      ~table() {
        // should we remove the shared_memory_object (by name) here as well?
        // segment is global so flushing should be manged by owner... 
        segment.flush();
      }

      // delete the rest of the gang don't ever want to copy a table -- but move?

      table(const table&) = delete;
      table(table&&) = delete;
      const table& operator=(const table&) = delete;
      const table& operator=(table&&) = delete;

      // printer

      friend std::ostream& operator<<(std::ostream& os, table& t) {
        os << "(" << (t.check_sanity() ? ":-) " : ":-( ") << t.tablename()
           << "[" << t.entries() << "] " <<  t.size() - t.get_free() << "/" << t.get_free() << "=" << t.size() << ")";
        return os;
      }  

      // insertion
      
      inline void insert(const std::string& k, const id_t& i) {
        db->insert(symbol(k.c_str(), i, allocator));
      }

      // todo put these implementations in cpp file?
      // xxx not sure how expensive these optional values are at runtime
      
      // lookup by name
      
      inline boost::optional<const symbol&> get_symbol(const char* k) {
        typedef typename symbol_table_t::template nth_index<0>::type symbol_by_name;
        
        symbol_by_name &name_idx = db->template get<0>();
        typename symbol_by_name::iterator i = name_idx.find(shared_string(k));
        if (i == name_idx.end()) return boost::none;
        else return *i;
      }

      // lookup by name

      inline boost::optional<const symbol&> get_symbol(const std::string& k) {
        typedef typename symbol_table_t::template nth_index<0>::type symbol_by_name;
        
        symbol_by_name& name_idx = db->template get<0>();
        typename symbol_by_name::iterator i = name_idx.find(shared_string(k));
        if (i == name_idx.end()) return boost::none;
        else return *i;
      }

      // lookup by index

      inline boost::optional<const symbol&> get_symbol(const id_t& k) {
        typedef typename symbol_table_t::template nth_index<1>::type symbol_by_id;
        
        symbol_by_id& id_idx = db->template get<1>(); 
        typename symbol_by_id::iterator i = id_idx.find(k);
        if (i == id_idx.end()) return boost::none;
        else return *i;
      }

      // XXX todo prefix btree/rb index lookup
      
      // delegated iterators

      typedef typename symbol_table_t::iterator iterator;
      typedef typename symbol_table_t::const_iterator const_iterator;
      
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
      const char*          name; 
      symbol_table_t*      db;
      segment_t&           segment;
      void_allocator_t     allocator;
    };
    
  }
}
