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

// topology wrapper

@implementation SDMTopology {
  molemind::sdm::database::space::topology* topo;
}

- (id)init: (molemind::sdm::database::space::topology*) tp {
  topo = tp;
  return self;
}

- (NSUInteger) card {
  return topo->size();
}
@end


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

// XXX UC add a symbol to a database space -- where is my enum?

- (NSInteger) addSymbolWithName: (NSString*) name
                        inSpace: (NSString*) space {
  
  // the semantics of ensure_symbol will ensure the space and add name to it
  molemind::sdm::database::status_t v = _sdm->ensure_symbol([space cStringUsingEncoding:NSUTF8StringEncoding],
                                                            [name cStringUsingEncoding:NSUTF8StringEncoding]);
  
  // true->added, false->existed, nil->fail
  // TODO better errors now we have codes.
  return v;
  //return (v>0) ? true : false;
} 


// get space cardinality -- correct for a non-existent space

- (NSUInteger) getSpaceCard: (NSString*) name {
  auto v = _sdm->get_space_cardinality([name cStringUsingEncoding:NSUTF8StringEncoding]);
  return (v ? (NSUInteger) *v : 0);
}

- (SDMTopology*) neighbourhoodOf: (NSString*) symbol
                       fromSpace: (NSString*) space
                         inSpace: (NSString*) target
                  aboveThreshold: (double) plower
                withDensityBelow: (double) rupper
              withMaxCardinality: (NSUInteger) card {
  auto maybe = _sdm->neighbourhood([target cStringUsingEncoding:NSUTF8StringEncoding],
                                   [space cStringUsingEncoding:NSUTF8StringEncoding],
                                   [symbol cStringUsingEncoding:NSUTF8StringEncoding], plower, rupper, card);
 
  return nullptr;
  /*
  if (maybe) {
    // totally bogus unsafe and silly -- who's managing this memory sunshine? oh yeah c++ ... whatever dark secrets the
    // c++ compiler shares between its functions (RTO) we are effectively saving a pointer to a stack allocated structure
    // except its not our stack i.e. XXX UNDEFINED BEHAVIOUR XXX
    // we would have to populate the SDMTopology here by copying the data before the frame goes out of scope!
    // using something like an NSMutableArray and a SDMPoint object.
    // this IS an FFI after all. s@molemind.net
    return [[SDMTopology alloc] init:&(*maybe)];
  }*/

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
