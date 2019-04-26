
using UnityEngine;
using System;
using System.Collections;

namespace Nanome.Core.Extension
{

    public static class FloatExtension
    {

        public static string toString(this float aFloat)
        {
            return "" + aFloat;
        }

        public static string toString(this float aFloat, int decimals)
        {
            if (decimals <= 0)
            {
                return "" + Mathf.RoundToInt(aFloat);
            }
            string format = "{0:F" + decimals + "}";
            return string.Format(format, aFloat);
        }

        public static int toInt(this float aFloat)
        {
            return Mathf.FloorToInt((float)aFloat);
        }

        public static float toFloat(this double aDouble)
        {
            return (float)aDouble;
        }

        public static double toDouble(this double aDouble)
        {
            return aDouble;
        }

    }

}
