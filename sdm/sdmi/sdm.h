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

NS_ASSUME_NONNULL_BEGIN

- (nullable instancetype) initWithName: (NSString*) name
                           initialSize: (NSUInteger) size
                               maxSize: (NSUInteger) max;

- (BOOL) addSymbolWithName: (NSString*) name
                                inSpace: (NSString*) space
                                  error: (NSError**) error;


- (NSUInteger) getSpaceCard: (NSString*) name;

- (BOOL)  giveMeSomethingWithLabel: (NSString*) label
                             error: (NSError **) error;


- (NSUInteger) getHeapSize;

- (NSUInteger) getFreeHeap;


NS_ASSUME_NONNULL_END

@end
