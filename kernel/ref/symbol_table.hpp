//
//  symbol_table.hpp
//  alligator
//
//  Created by datalligator on 06/03/2013.
//  Copyright (c) 2013 Simon Beaumont. All rights reserved.
//

#ifndef alligator_symbol_table_hpp
#define alligator_symbol_table_hpp

#include <iostream>
#include <fstream>
#include <string>

#include <boost/intrusive/unordered_set.hpp>
#include <boost/intrusive/set.hpp>
#include <boost/functional/hash.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/unordered_map.hpp>
#include <boost/tokenizer.hpp>
#include <boost/format.hpp>
#include <boost/algorithm/string.hpp>

#include "space.hpp"
#include "score.hpp"


// worlds are namespaces for vectors with sets of symbols that can be quickly looked up

namespace symtab {
  
  
  class undefined_symbol : public std::runtime_error {
  public:
    undefined_symbol(const std::string& what_arg) : std::runtime_error(what_arg) {}
    undefined_symbol(const char* what_arg) : std::runtime_error(what_arg) {}
  };

  class path_error : public std::runtime_error {
  public:
    path_error(const std::string& what_arg) : std::runtime_error(what_arg) {}
    path_error(const char* what_arg) : std::runtime_error(what_arg) {}
  };

  
  using namespace boost::intrusive;
  using namespace boost::algorithm;
  
  // intrusive set member
  
  struct symbol final : public set_base_hook<optimize_size<true>> {
    
    enum type {NOP, ZERO, RAND, UNIT} ;
    
    symbol(const std::string& s) : value(s) {}
    symbol(const std::string& s, const std::size_t i) : value(s), id(i) {}
    
    friend bool operator< (const symbol &a, const symbol &b)
    {  return a.value < b.value;  }
    friend bool operator> (const symbol &a, const symbol &b)
    {  return a.value > b.value;  }
    friend bool operator== (const symbol &a, const symbol &b)
    {  return a.value == b.value;  }
    
    std::string value;
    std::size_t id;
  };
  
  // cleanup when deleteing from instrusive container needs a reaper
  
  struct symbol_disposer {
    void operator()(symbol* delete_this) { delete delete_this; }
  };

  // worlds are symbol tables that have a dedicated parametric space
  
  template <typename S, typename U>
  struct world final {
    
    const std::string name;
    
    // c'tor
    world(const std::string& n, S* s, U& u) : name(n), space(s), universe(u) {}
    
    ~world() {
      delete space;
      table.clear_and_dispose(symbol_disposer());
      std::cout << "world destroyed" << std::endl;
    }
    
    typedef typename S::base_vector_t vector_t;
    typedef set<symbol, compare<std::less<symbol>>> base_table;
    
   
    inline void set_sparsity(float const& p) {
      standard_sparsity = (p > 0 && p < 1.) ? p : universe.default_sparsity;
    }
   
    inline const std::size_t cardinality() {
      return space->size();
    }
    
    ///////////////////////////
    // symbol+vector factory //
    ///////////////////////////
    
    inline vector_t* const ensure_symbol2(std::string const& worldname,
                                          std::string const& s,
                                          const symbol::type t = symbol::type::NOP,
                                          const float p = 0.0)  {
      
      // get symbol's world
      world* wp = universe.get_world(worldname);
    
      // lookup symbol in the world
      auto hits = wp->table.find(s);
      
      symbol* sp = nullptr;
      vector_t* v = nullptr;
      
      if (hits == wp->table.end()) {
        // not found so create and initialize unless lookup only
        if (t == symbol::type::NOP) throw undefined_symbol("undefined symbol: " + s);
        
        // allocate new vector and symbol
        const std::size_t i = wp->space->next_vector_id();
        sp = new symbol(s, i);
        v = wp->space->new_vector(sp);
        wp->table.insert(*sp);
        
        // initialize vector IFF newly created
        switch (t) {
          case symbol::type::NOP:
            break;
          case symbol::type::ZERO:
            v->zero();
            break;
          case symbol::type::RAND:
            v->zero();
            wp->space->randomize_vector(v, (p>0 && p<1.) ? p : standard_sparsity);
            break;
          case symbol::type::UNIT:
            v->unit();
            break;
        }
        
      } else {
        // found so just return the vector
        sp = &*hits;
        v = wp->space->get_vector(sp->id);
      }
      
      return v;
    }

    
    // name functions
    
    inline std::vector<std::string> const get_names(std::string const& s) const {
      std::vector<std::string> matches;
      symbol sym = symbol(s);
      for (auto iter = table.lower_bound(sym); iter != table.end(); iter++) {
        if (!starts_with(iter->value, sym.value)) break;
        else matches.push_back(iter->value);
      }
      return matches;
    }
    
    
    // vector ops
    
    inline double const get_density(std::string const& w, std::string const& s) {
      return ensure_symbol2(w, s)->density();
    }
    
    inline double const add(std::string const& lw, std::string const& lhs, std::string const& rw, std::string const& rhs)  {
      auto u = ensure_symbol2(lw, lhs);
      auto v = ensure_symbol2(rw, rhs);
      u->adda(v);
      return u->density();
    }
   
    
    inline double const mul(std::string const& lw, std::string const& lhs, std::string const& rw, std::string const& rhs)  {
      auto u = ensure_symbol2(lw, lhs);
      auto v = ensure_symbol2(rw, rhs);
      u->mula(v);
      return u->density();
    }
    
    inline double const sub(std::string const& lw, std::string const& lhs, std::string const& rw, std::string const& rhs)  {
      auto u = ensure_symbol2(lw, lhs);
      auto v = ensure_symbol2(rw, rhs);
      u->suba(v);
      return u->density();
    }
    
    inline std::vector<dsm::score<symbol>> const neighbours(std::string const& w, std::string const& s, const float p, const float d, const std::size_t n)  {
      auto v = ensure_symbol2(w, s);
      return space->neighbourhood(v, p, d, n);
    }
    
    inline double const similarity(std::string const& lw, std::string const& lhs, std::string const& rw, std::string const& rhs)  {
      auto u = ensure_symbol2(lw, lhs);
      auto v = ensure_symbol2(rw, rhs);
      return u->similarity(v);
    }
    
    inline double const inner(std::string const& lw, std::string const& lhs, std::string const& rw, std::string const& rhs)  {
      auto u = ensure_symbol2(lw, lhs);
      auto v = ensure_symbol2(rw, rhs);
      return u->inner(v);
    }
    
    inline double const countsum(std::string const& lw, std::string const& lhs, std::string const& rw, std::string const& rhs)  {
      auto u = ensure_symbol2(lw, lhs);
      auto v = ensure_symbol2(rw, rhs);
      return u->countsum(v);
    }
    
    inline double const distance(std::string const& lw, std::string const& lhs, std::string const& rw, std::string const& rhs)  {
      auto u = ensure_symbol2(lw, lhs);
      auto v = ensure_symbol2(rw, rhs);
      return u->distance(v);
    }

    // experimental: dump all symbols and vectors in the space
     
    void save() {
      
      std::ofstream outs(name);
      
      for (auto iter = table.begin(); iter != table.end(); ++iter) {
        outs << iter->value << "\t" << space->get_vector(iter->id)->count() << std::endl;
      }
    }
    
    // assert training tuple
    
    void inline assert_data(std::string const& lw, std::string const& lhs, std::string const& rw, std::string const& rhs) {
      auto u = ensure_symbol2(lw, lhs, symtab::symbol::type::ZERO);
      auto v = ensure_symbol2(rw, rhs, symtab::symbol::type::RAND, standard_sparsity);
      u->adda(v);

    }
    
    
    // load lexicon file
    
    inline const std::string load_lexicon(const std::string& path) {
      
      std::ifstream ins(path);
      std::string line;
      //std::size_t lines = 0;
      
      if (not ins) throw path_error(path + ": not found!");
      else {
        
        while(std::getline(ins, line)) {
          //++lines;
          boost::trim(line);
          ensure_symbol2(name, line, symtab::symbol::type::ZERO, 0);
        }
    
        return path;
      }
    }
    
    
  public:
    float standard_sparsity;
  private:
    U& universe;
    base_table table;
    S* space;
  };
  
  
  
  // the universe... where spaces are implemented and worlds collide!
  
  struct world_map final {
    
    typedef dsm::vectorspace<symtab::symbol, 1024> space_t;
    typedef symtab::world<space_t, world_map> world_t;
    typedef boost::unordered_map<const std::string, world_t*> world_map_t;
    
    world_map(const std::string& name, const std::size_t& size, float const& p) : default_world(make_world(name, size, *this, p)), default_sparsity(p) {}
    
    // world factory
    inline world_t* make_world(const std::string& name, const std::size_t& size, world_map &u, float const& p = 0.) {
      auto iter = worlds.find(name);
      if (iter != worlds.end()) throw undefined_symbol("world already defined: " + name);
      space_t* space = new space_t(size);
      world_t* world = new world_t(name, space, u);
      add_world(name, world);
      world->set_sparsity((p > 0 && p < 1) ? p : default_sparsity);
      return world;
    }
    

    inline void add_world(const std::string& name, world_t* wrld) {
      worlds.insert(std::pair<std::string, world_t*>(name, wrld));
    }
    
    inline world_t* get_world(const std::string& name) {
      auto iter = worlds.find(name);
      if (iter == worlds.end()) {
        // XXX experimental feature
        std::cout << "[auto create world: " << name << "]" << std::endl;
        return make_world(name, 10000, *this);
      } else return iter->second;
    }

    inline std::vector<std::string> const get_worlds() const {
      std::vector<std::string> world_names;
      for (auto i: worlds)
        world_names.push_back(i.first);
      return world_names;
    }
    
    inline world_t* get_default_world() const {
      return default_world;
    }
    
    inline void delete_world(std::string const& name) {
      auto iter = worlds.find(name);
      if (iter == worlds.end()) throw undefined_symbol("undefined world: " + name);
      worlds.erase(iter);
      delete iter->second;
    }
    
  public:
    float default_sparsity;
  private:
    world_map_t worlds;
    // XXX I think default world might be deprecated now.
    world_t* default_world;
  };
  

    

}

#endif
