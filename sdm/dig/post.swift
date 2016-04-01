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
      NSLog("removed database: %s", filepath)
    } catch {
      // more specific please!
      NSLog("failed to remove database: %s", filepath)
    }
  } else {
   NSLog("no database to remove: %s", filepath)
  }
}

// derived utility functions

func createSDMTestDatabase(sizeMb: UInt, maxMb: UInt) -> SDMDatabase {
  return createSDMDatabase(".POST", sizeMb: sizeMb, maxMb: maxMb)
}


func destroySDMTestDatabase() -> Void {
  return destroySDMDatabase(".POST")
}


/* 
The plan here is to create a database then try and grow it in chunks
populatingchunks as we go and calibrating the number of vectors we can
allocate per chunk before running out of memory.
*/

func postRun() -> Void {
  createSDMTestDatabase(20, maxMb: 700)
}