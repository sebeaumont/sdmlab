//
//  sdm.h
//  sdm
//
//  Created by Simon Beaumont on 22/03/2016.
//  Copyright © 2016 Simon Beaumont. All rights reserved.
//
#import <Foundation/Foundation.h>

//! SDM Database 
@interface SDMDatabase : NSObject

- (instancetype) initWithName: (NSString*) name
                         size: (NSUInteger) size
                          max: (NSUInteger) max;

- (bool) addSymbolWithName: (NSString*) name
                   inSpace: (NSString*) space;

- (NSUInteger) getSpaceCard: (NSString*) name;

@end
