//
//  FirstViewController.swift
//  dig
//
//  Created by Simon Beaumont on 21/03/2016.
//  Copyright © 2016 Simon Beaumont. All rights reserved.
//

import UIKit


class FirstViewController: UIViewController {
  
  override func viewDidLoad() {
    super.viewDidLoad()
    // Do any additional setup after loading the view, typically from a nib.
  }

  override func didReceiveMemoryWarning() {
    super.didReceiveMemoryWarning()
    // Dispose of any resources that can be recreated.
    
  }

  
  @IBAction func testAction(sender: AnyObject) {
    postRun()
  }

}

