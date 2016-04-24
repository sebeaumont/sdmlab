//
//  sdm.h
//  sdm
//
//  Created by Simon Beaumont on 22/03/2016.
//  Copyright © 2016 Simon Beaumont. All rights reserved.
//
#import <Foundation/Foundation.h>
//#include "database.hpp"



//! SDM Database wrapper for Swift/ObjectiveC/C API on OSX derived systems

@interface SDMDatabase : NSObject

NS_ASSUME_NONNULL_BEGIN

- (nullable instancetype) initWithName: (NSString*) name
                           initialSize: (NSUInteger) size
                               maxSize: (NSUInteger) max;

- (BOOL) addSymbolWithName: (NSString*) name
                   inSpace: (NSString*) space
                     error: (NSError**) error;


- (NSUInteger) getSpaceCard: (NSString*) name;




/*
/// find by prefix
std::pair<space::symbol_iterator, space::symbol_iterator> search_symbols(const std::string& space_name, const std::string& symbol_prefix) noexcept;
*/

/*
/// randomise a vector
void randomize_vector(boost::optional<space::vector&> vector, double p) noexcept;

/// ones
void ones_vector(boost::optional<space::vector&> v) noexcept;

/// zeros
void zeros_vector(boost::optional<space::vector&> v) noexcept;

// get space
space* get_space_by_name(const std::string&);

/// database memoizes pointers to named spaces to optimize symbol resolution
space* ensure_space_by_name(const std::string&);


/////////////////////////
/// vector properties ///
/////////////////////////

/// get vector density
boost::optional<const double> density(const std::string& space_name, const std::string& vector_name) noexcept;


/////////////////////////////////////////////////////
/// effectful learning operations on target vectors
/////////////////////////////////////////////////////

/// add or superpose
void superpose(const std::string& ts, const std::string& tn,
               const std::string& ss, const std::string& sn) noexcept;

/// subtract
void subtract(const std::string& ts, const std::string& tn,
              const std::string& ss, const std::string& sn) noexcept;

/// multiply
void multiply(const std::string& ts, const std::string& tn,
              const std::string& ss, const std::string& sn) noexcept;

/// TODO exponents

/// TODO shifts and other permutations of bases


////////////////////////
/// vector measurement
////////////////////////

/// simlilarity (unit distance)
boost::optional<double> similarity(const std::string&, const std::string&,
                                   const std::string&, const std::string&) noexcept;

/// inner product
boost::optional<double> inner(const std::string&, const std::string&,
                              const std::string&, const std::string&) noexcept;

////////////////////
/// find vectors ///
////////////////////

/// toplogy of n nearest neighbours satisfying p, d constraints
boost::optional<database::space::topology> neighbourhood(const std::string& target_space,
                                                         const std::string& source_space,
                                                         const std::string& source_name,
                                                         double similarity_lower_bound,
                                                         double density_upper_bound,
                                                         std::size_t cardinality_upper_bound) noexcept;
/// TODO negation

/// TODO query algebra


////////////////////////
/// space operations ///
////////////////////////

bool destroy_space(const std::string&) noexcept;

std::vector<std::string> get_named_spaces() noexcept;

boost::optional<std::size_t> get_space_cardinality(const std::string&) noexcept;

/// TODO space operators


//////////////////////
/// heap utilities ///
//////////////////////

bool grow_heap_by(const std::size_t&) noexcept;

bool compactify_heap() noexcept;

/// heap metrics

inline std::size_t heap_size() noexcept { return heap.get_size(); }
inline std::size_t free_heap() noexcept { return heap.get_free_memory(); }
inline bool check_heap_sanity() noexcept { return heap.check_sanity(); }
inline bool can_grow_heap() noexcept { return (heap.get_size() < maxheap); }
*/

- (NSUInteger) getHeapSize;

- (NSUInteger) getFreeHeap;


NS_ASSUME_NONNULL_END

@end
