//
//  RIVAppDelegate.m
//  InterfaceSimulator
//
//  Created by Philip White on 4/7/12.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

#import "RIVAppDelegate.h"

@implementation RIVAppDelegate

@synthesize window = _window;

- (void)dealloc
{
    [super dealloc];
}

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification
{
    // Insert code here to initialize your application
    [NSTimer scheduledTimerWithTimeInterval:0.5 target:self selector:@selector(flashSelection:) userInfo:nil repeats:YES];
}

- (void)flashSelection:(NSTimer*)timer
{
    NSRange sr = [tv selectedRange];
    
    if (sr.length != 0)
        [tv showFindIndicatorForRange:sr];
}

- (NSInteger)numberOfRowsInTableView:(NSTableView *)aTableView
{
    return 5;
}

- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)aTableColumn row:(NSInteger)rowIndex
{
    NSString *issues[] = {@"Core Modal Overuse", @"Inflected Genitive Underuse", @"Phrasal Verb Underuse", @"Pronominal Argument Overuse", @"Passive Voice Overuse"};
    
    if (rowIndex >= 0 && rowIndex < 5)
        return issues[rowIndex];
    else
        return @"";
}

@end
