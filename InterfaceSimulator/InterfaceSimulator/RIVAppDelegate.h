//
//  RIVAppDelegate.h
//  InterfaceSimulator
//
//  Created by Philip White on 4/7/12.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@interface RIVAppDelegate : NSObject <NSApplicationDelegate,NSTableViewDataSource>
{
    IBOutlet NSTextView *tv;
}
@property (assign) IBOutlet NSWindow *window;



@end
