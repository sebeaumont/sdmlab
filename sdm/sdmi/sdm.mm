//
//  sdmi.mm
//  sdm
//
//  Created by Simon Beaumont on 22/03/2016.
//  Copyright © 2016 Simon Beaumont. All rights reserved.
//

// objective-c++ wrapper for c++ sdm library


#import "sdm.h"
#include "database.hpp"

NSString* errorDomain = @"net.molemind.dig.error";


// wrap c++ object
@interface SDMDatabase()
@property (nonatomic, readwrite, assign) molemind::sdm::database* sdm;
@end

@implementation SDMDatabase
@synthesize sdm = _sdm;

// constructor
- (instancetype) initWithName:(NSString*) name
                         size:(NSUInteger) size
                          max:(NSUInteger) max {
  // init sdm database object N.B. this can fail 
  if (self = [super init]) {
    _sdm = new molemind::sdm::database((std::size_t) size,
                                       (std::size_t) max,
                                       [name cStringUsingEncoding:NSUTF8StringEncoding]);
  }
  return self;
}

// TODO methods...

// add a symbol to a database space
- (NSInteger) addSymbol: (NSString*) name
                  space: (NSString*) space {
  auto v = _sdm->add_symbol([space cStringUsingEncoding:NSUTF8StringEncoding],
                            [name cStringUsingEncoding:NSUTF8StringEncoding]);
  if (v) return (NSInteger) *v;
  else return -1;
}

// get space card

- (NSInteger) getSpaceCard: (NSString*) name {
  auto v = _sdm->get_space_cardinality([name cStringUsingEncoding:NSUTF8StringEncoding]);
  return v;
}

// destructor
- (void)dealloc {
  delete _sdm;
}


@end