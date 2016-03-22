//
//  FirstViewController.swift
//  dig
//
//  Created by Simon Beaumont on 21/03/2016.
//  Copyright © 2016 Simon Beaumont. All rights reserved.
//

import UIKit
import sdmi

func getDocumentsURL() -> NSURL {
  let documentsURL = NSFileManager.defaultManager().URLsForDirectory(.DocumentDirectory, inDomains: .UserDomainMask)[0]
  return documentsURL
}


func fileInDocumentsDirectory(filename: String) -> String {
  
  let fileURL = getDocumentsURL().URLByAppendingPathComponent(filename)
  return fileURL.path!
  
}

class FirstViewController: UIViewController {

  let Mb : UInt = 1024 * 1024;
  
  override func viewDidLoad() {
    super.viewDidLoad()
    // Do any additional setup after loading the view, typically from a nib.
    let db = SDMDatabase(name: fileInDocumentsDirectory("test.sdm"), size: 10*Mb, max: 10*Mb)
    print(db);
    // built in unit tests -- hohum
    db.addSymbol("Simon", space:"People")
    db.addSymbol("Hazel", space:"People")
    // get symbols/vectors
  }

  override func didReceiveMemoryWarning() {
    super.didReceiveMemoryWarning()
    // Dispose of any resources that can be recreated.
  }


}

