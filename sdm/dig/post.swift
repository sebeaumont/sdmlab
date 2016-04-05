//
//  post.swift
//  sdm
//
//  Created by Simon Beaumont on 31/03/2016.
//  Copyright © 2016 Simon Beaumont. All rights reserved.
//

import Foundation


// These are the Power On Self Tests for the dig app
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

func createSDMDatabase(filename: String, sizeMb: UInt, maxMb: UInt) -> SDMDatabase {
  let Mb : UInt = 1024 * 1024
  // create sdm
  let db = SDMDatabase(name: fileInDocumentsDirectory(filename), size: sizeMb*Mb, max: maxMb*Mb)
  // logging anyone?
  NSLog("created database: %@ %d/%d", db, sizeMb, maxMb)
  return db
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
  
  // stat file
  if let filestats = statFile(filename) {
    NSLog("file stats: %@", filestats)
    destroySDMDatabase(filename)
  }
  
  let db = createSDMDatabase(filename, sizeMb: 500, maxMb: 700)
  let nv = probeSymbolCardinality(db, testspace: testspace)

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
    let sid = db.addSymbol("symbol-\(card)", space: testspace)
    
    if sid >= 0 {
      card += 1
      if card % 10000 == 0 {
        NSLog("added: %d", card)
      }
      
    } else {
      NSLog("limit: %d, (%d)", card, sid)
      return card
    }
  }
}
