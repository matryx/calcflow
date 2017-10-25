//
//  VBUtilityBinding.m
//  Utility
//
//  Created by Ashwin kumar on 20/03/15.
//  Copyright (c) 2015 Voxel Busters Interactive LLP. All rights reserved.
//

#import "VBUtilityBinding.h"

char* NSStringToUnityString (NSString *string)
{
	if (string == NULL)
		return NULL;
	
	const char* cString	= [string UTF8String];
	
	// Copy c string
	char *uString		= (char*)malloc(strlen(cString) + 1);
	strcpy(uString, cString);
	
	return uString;
}

char* utilityBundleVersion ()
{
	NSString *version = [[NSBundle mainBundle] objectForInfoDictionaryKey:@"CFBundleShortVersionString"];
	
	return NSStringToUnityString(version);
}

char* utilityBundleIdentifier ()
{
	return NSStringToUnityString([[NSBundle mainBundle] bundleIdentifier]);
}