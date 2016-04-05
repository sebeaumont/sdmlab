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

- (NSInteger) addSymbol: (NSString*) name
                  space: (NSString*) space;

- (NSInteger) getSpaceCard: (NSString*) name;

@end
