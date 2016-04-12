//
//  post.swift
//  sdm
//
//  Created by Simon Beaumont on 31/03/2016.
//  Copyright © 2016 Simon Beaumont. All rights reserved.
//

import Foundation

//////////////////////////////////////////////////////
// These are the Power On Self Tests for the dig app

///////////////////////////
// !!! HARD HAT AREA !!! //
///////////////////////////

import sdmi


// helper functions

func getDocumentsURL() -> NSURL {
  let documentsURL = NSFileManager.defaultManager().URLsForDirectory(.DocumentDirectory, inDomains: .UserDomainMask)[0]
  return documentsURL
}


func fileInDocumentsDirectory(filename: String) -> String {
  let fileURL = getDocumentsURL().URLByAppendingPathComponent(filename)
  return fileURL.path!
}

func statFile(filename: String) -> [String : AnyObject]? {
  let filepath = fileInDocumentsDirectory(filename)
  return try? NSFileManager.defaultManager().attributesOfItemAtPath(filepath)
}


// sdm helpers

func createSDMDatabase(filename: String, sizeMb: UInt, maxMb: UInt) -> SDMDatabase? {
  let Mb : UInt = 1024 * 1024
  // create sd
  return SDMDatabase(name: fileInDocumentsDirectory(filename), initialSize: sizeMb*Mb, maxSize: maxMb*Mb)
}


func destroySDMDatabase(filename: String) -> Void {
  let manager = NSFileManager.defaultManager()
  let filepath = fileInDocumentsDirectory(filename)
  
  if manager.fileExistsAtPath(filepath) {
    do {
      try manager.removeItemAtPath(filepath)
      NSLog("removed database: %@", filepath)
    } catch {
      // more specific please!
      NSLog("failed to remove database: %@", filepath)
    }
  } else {
   NSLog("no database to remove: %@", filepath)
  }
}



/* 
The plan here is to create a database then try and grow it in chunks
populatingchunks as we go and calibrating the number of vectors we can
allocate per chunk before running out of memory.
*/

func postRun() -> Void {
  
  let filename = ".POST"
  let testspace = "Test"
  let iniSizeMb : UInt = 500
  let maxSizeMb : UInt = 700
  
  // stat file
  if let filestats = statFile(filename) {
    NSLog("file stats: %@", filestats)
    destroySDMDatabase(filename)
  }
  
  if let db = createSDMDatabase(filename, sizeMb: iniSizeMb, maxMb: maxSizeMb) {
    NSLog("created database: %@ %d/%d", db, iniSizeMb, maxSizeMb)
    let card = probeSymbolCardinality(db, testspace: testspace)
    // testing, testing, 1, 2, 3...
    NSLog("testspace: %@:%d", testspace, card)
    
    if let foo = try? db.giveMeSomethingWithLabel(testspace) {
      print("foo: \(foo)", foo)
    }
    
  } else {
    NSLog("failed database: %@ %d/%d", filename, iniSizeMb, maxSizeMb)
  }
  
  //
  destroySDMDatabase(filename)
}


// insert symbols into test space

func probeSymbolCardinality(db: SDMDatabase, testspace: String) -> UInt {
  let start = db.getSpaceCard(testspace)
  // TODO get current cardianality of space and add
  //let card = db.getSpaceCardinality(testspace)
  NSLog("starting load: %@ [%d]", testspace, start)
  var card: UInt = start
  
  while true {
    
    if let foo = try? db.addSymbolWithName("symbol-\(card)", inSpace: testspace) {
        card += 1
        if card % 10000 == 0 {
          // TODO: some heap stats
          NSLog("added: \(card) \(foo)")
        }
        
    } else {
      
      NSLog("limit: %d", card)
      return card
    }
  }
}
