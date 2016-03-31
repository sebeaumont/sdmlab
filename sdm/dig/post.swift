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


func createSDMTestDatabase(filename: String, sizeMb: UInt, maxMb: UInt) -> SDMDatabase {
  let Mb : UInt = 1024 * 1024;
  // create sdm
  let db = SDMDatabase(name: fileInDocumentsDirectory(filename), size: sizeMb*Mb, max: maxMb*Mb)
  print(db)
  // built in unit tests -- hohum :-)
  db.addSymbol("Simon", space:"People")
  db.addSymbol("Hazel", space:"People")
  // get symbols/vectors
  return db
}
