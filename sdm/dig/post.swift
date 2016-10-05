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

func getDocumentsURL() -> URL {
  let documentsURL = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0]
  return documentsURL
}


func fileInDocumentsDirectory(_ filename: String) -> String {
  let fileURL = getDocumentsURL().appendingPathComponent(filename)
  return fileURL.path
}


func statFile(_ filename: String) -> [FileAttributeKey: Any]? {
  let filepath = fileInDocumentsDirectory(filename)
  return try? FileManager.default.attributesOfItem(atPath: filepath)
}


// sdm helpers

func createSDMDatabase(_ filename: String, sizeMb: UInt, maxMb: UInt) -> SDMDatabase? {
  let Mb : UInt = 1024 * 1024
  // create sd
  return SDMDatabase(name: fileInDocumentsDirectory(filename), initialSize: sizeMb*Mb, maxSize: maxMb*Mb)
}


func destroySDMDatabase(_ filename: String) -> Void {
  let manager = FileManager.default
  let filepath = fileInDocumentsDirectory(filename)
  
  if manager.fileExists(atPath: filepath) {
    do {
      try manager.removeItem(atPath: filepath)
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
    
    /*
    if let foo = try? db.giveMeSomethingWithLabel(testspace) {
      print("foo: \(foo)", foo)
    }*/
    
  } else {
    NSLog("failed database: %@ %d/%d", filename, iniSizeMb, maxSizeMb)
  }
  
  //
  destroySDMDatabase(filename)
}


// insert symbols into test space

func probeSymbolCardinality(_ db: SDMDatabase, testspace: String) -> UInt {
  let start = db.getSpaceCard(testspace)
  // TODO get current cardianality of space and add
  //let card = db.getSpaceCardinality(testspace)
  NSLog("starting load: %@ [%d]", testspace, start)
  var card: UInt = start
  
  while true {
    
    if let _ = try? db.addSymbol(withName: "symbol-\(card)", inSpace: testspace) {
      card += 1
      if card % 10000 == 0 {
        let free = db.getFreeHeap()
        let heap = db.getHeapSize()
        NSLog("added: \(card) heap: \(heap) free: \(free)")
      }
        
    } else {
      let free = db.getFreeHeap()
      let heap = db.getHeapSize()
      NSLog("failed: \(card) heap: \(heap) free: \(free)")
      return card
    }
  }
}
