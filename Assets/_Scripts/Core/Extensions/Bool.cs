
using UnityEngine;
using System;
using System.Collections;

namespace Nanome.Core.Extension
{

    public static class BoolExtension
    {

        public static string toString(this bool aBool)
        {
            if (aBool)
            {
                return "1";
            }
            return "0";
        }

        public static int toInt(this bool aBool)
        {
            if (aBool)
            {
                return 1;
            }
            return 0;
        }

        public static bool toBool(this bool aBool)
        {
            return aBool;
        }

        public static float toFloat(this bool aBool)
        {
            if (aBool)
            {
                return 1f;
            }
            return 0f;
        }

        public static double toDouble(this bool aBool)
        {
            if (aBool)
            {
                return 1.0;
            }
            return 0.0;
        }

    }

}
