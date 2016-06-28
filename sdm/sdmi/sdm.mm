//
//  sdmi.mm
//  sdm
//
//  Created by Simon Beaumont on 22/03/2016.
//  Copyright © 2016 Simon Beaumont. All rights reserved.
//

// objective-c++ wrapper for c++ sdm library  sdmi.framework


#import "sdm.h"
#include "database.hpp"

NSString* errorDomain = @"net.molemind.dig.error";


// wrap c++ database object

@interface SDMDatabase()
@property (nonatomic, readonly, assign) molemind::sdm::database* sdm;
@end

@implementation SDMDatabase
@synthesize sdm = _sdm;

// constructor

- (instancetype) initWithName:(NSString*) name
                  initialSize:(NSUInteger) size
                      maxSize:(NSUInteger) max {
  // init sdm database object XXX this can throw
  if (self = [super init]) {
    _sdm = new molemind::sdm::database((std::size_t) size,
                                       (std::size_t) max,
                                       [name cStringUsingEncoding:NSUTF8StringEncoding]);
  }
  return self;
}


// destructor

- (void) dealloc {
  delete _sdm;
}


/* 
 TODO: mostly adapting boost optionals and string encoding for c++ methods
 maybe we should only return nullables which Swift should see as optionals?
*/

// XXX UC add a symbol to a database space -- ensure_symbol is tristate optional<bool>
// -- wee need a nullable bool...
// this will throw in Swift but returns void

- (BOOL) addSymbolWithName: (NSString*) name
                   inSpace: (NSString*) space
                     error: (NSError**) error {
  
  // the semantics of ensure_symbol will ensure the space and add name to it
  database::status_t v = _sdm->ensure_symbol([space cStringUsingEncoding:NSUTF8StringEncoding],
                                             [name cStringUsingEncoding:NSUTF8StringEncoding]);
  
  // true->added, false->existed, nil->fail
  // TODO better errors now we have codes.
  return (v>0) ? true : false;
} 


// get space cardinality -- correct for a non-existent space

- (NSUInteger) getSpaceCard: (NSString*) name {
  auto v = _sdm->get_space_cardinality([name cStringUsingEncoding:NSUTF8StringEncoding]);
  return (v ? (NSUInteger) *v : 0);
}


// dummy one for to test this error malarkey

- (BOOL) giveMeSomethingWithLabel: (NSString*) label
                            error: (NSError **) error {
  return false;
}

- (NSUInteger) getHeapSize {
  return _sdm->heap_size();
}

- (NSUInteger) getFreeHeap {
  return _sdm->free_heap();
}

@end