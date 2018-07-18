//
//  iOSIdentifiers.m
//
#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>
#include <cstring>

extern "C" char* mixpanel_ios_get_idfa()
{
    Class ASIdentifierManagerClass = NSClassFromString(@"ASIdentifierManager");
    if (ASIdentifierManagerClass) {
        SEL sharedManagerSelector = NSSelectorFromString(@"sharedManager");
        id sharedManager = ((id (*)(id, SEL))[ASIdentifierManagerClass methodForSelector:sharedManagerSelector])(ASIdentifierManagerClass, sharedManagerSelector);

        SEL isAdvertisingTrackingEnabledSelector = NSSelectorFromString(@"isAdvertisingTrackingEnabled");
        BOOL isAdvertisingTrackingEnabled = ((BOOL (*)(id, SEL))[sharedManager methodForSelector:isAdvertisingTrackingEnabledSelector])(sharedManager, isAdvertisingTrackingEnabledSelector);

        if (isAdvertisingTrackingEnabled) {
            SEL advertisingIdentifierSelector = NSSelectorFromString(@"advertisingIdentifier");
            NSUUID *uuid = ((NSUUID* (*)(id, SEL))[sharedManager methodForSelector:advertisingIdentifierSelector])(sharedManager, advertisingIdentifierSelector);

            char* ret = new char[100];
            ret[0] = 0;
            strncpy(ret, [[uuid UUIDString] UTF8String], 100);
            ret[99] = 0;

            return ret;
        }
    }

    return "";
}
