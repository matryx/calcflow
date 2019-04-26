using UnityEngine;

using System;
using System.Collections;
using System.Collections.Generic;
using System.Threading;

using Nanome.Core;

namespace Nanome.Core
{

    public class ThreadLocalBuffers
    {

        [ThreadStatic]
        static char[] charArray = null;
        public static char[] chars()
        {
            if (charArray == null)
            {
                charArray = new char[1024];
            }
            return charArray;
        }

        [ThreadStatic]
        static byte[] byteArray = null;
        public static byte[] bytes()
        {
            if (byteArray == null)
            {
                byteArray = new byte[1024];
            }
            return byteArray;
        }

        [ThreadStatic]
        static short[] shortArray = null;
        public static short[] shorts()
        {
            if (shortArray == null)
            {
                shortArray = new short[32];
            }
            return shortArray;
        }

        [ThreadStatic]
        static ushort[] ushortArray = null;
        public static ushort[] ushorts()
        {
            if (ushortArray == null)
            {
                ushortArray = new ushort[32];
            }
            return ushortArray;
        }

        [ThreadStatic]
        static int[] intArray = null;
        public static int[] ints()
        {
            if (intArray == null)
            {
                intArray = new int[32];
            }
            return intArray;
        }

        [ThreadStatic]
        static long[] longArray = null;
        public static long[] longs()
        {
            if (longArray == null)
            {
                longArray = new long[32];
            }
            return longArray;
        }

        [ThreadStatic]
        static float[] floatArray = null;
        public static float[] floats()
        {
            if (floatArray == null)
            {
                floatArray = new float[32];
            }
            return floatArray;
        }

        [ThreadStatic]
        static double[] doubleArray = null;
        public static double[] doubles()
        {
            if (doubleArray == null)
            {
                doubleArray = new double[32];
            }
            return doubleArray;
        }

    }

}
