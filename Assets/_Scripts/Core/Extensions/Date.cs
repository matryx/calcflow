
using UnityEngine;
using System;
using System.Collections;

namespace Nanome.Core.Extension
{

    public static class DateExtension
    {

        public static double ToTimestampMs(this DateTime d)
        {
            var utcTime = d.ToUniversalTime();
            var epochTime = new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc);
            var relativeTime = utcTime.Subtract(epochTime);
            return relativeTime.TotalMilliseconds;
        }

        public static double ToTimestamp(this DateTime d)
        {
            var utcTime = d.ToUniversalTime();
            var epochTime = new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc);
            var relativeTime = utcTime.Subtract(epochTime);
            return relativeTime.TotalSeconds;
        }

        public static DateTime TimestampToDateTime(this double unixTimeStamp)
        {
            var epochTime = new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc);
            var relativeTime = epochTime.AddSeconds(unixTimeStamp);
            return relativeTime.ToLocalTime();
        }

        public static DateTime TimestampMstoDateTime(this double unixTimeStampMs)
        {
            var epochTime = new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc);
            var relativeTime = epochTime.AddMilliseconds(unixTimeStampMs);
            return relativeTime.ToLocalTime();
        }

    }

}
