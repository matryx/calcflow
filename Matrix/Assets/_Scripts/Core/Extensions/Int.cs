
using UnityEngine;
using System;
using System.Collections;

namespace Nanome.Core.Extension
{

    public static class IntExtension
    {

        public static ulong RotateLeft(this ulong original, int bits)
        {
            return (original << bits) | (original >> (64 - bits));
        }

        public static ulong RotateRight(this ulong original, int bits)
        {
            return (original >> bits) | (original << (64 - bits));
        }

        unsafe public static ulong GetUInt64(this byte[] bb, int pos)
        {
            // we only read aligned longs, so a simple casting is enough
            fixed (byte* pbyte = &bb[pos])
            {
                return *((ulong*)pbyte);
            }
        }

        public static string toString(this int[] anArray)
        {
            string str = "";
            if (anArray != null)
            {
                if (anArray.Length > 0)
                {
                    str = str + anArray[0];
                }
                for (int i = 1; i < anArray.Length; i++)
                {
                    if (anArray.Length <= 0)
                    {
                        return str;
                    }
                    str = str + "," + anArray[i];
                }
            }
            return str;
        }

        public static bool ContentsEqualTo(this int[] anArray, int[] array2)
        {
            for (int i = 0; i < anArray.Length; i++)
            {
                if (array2.Length <= i || anArray[i] != array2[i])
                {
                    return false;
                }
            }
            return true;
        }

        public static string toString(this int anInt)
        {
            return "" + anInt;
        }

        public static int toInt(this int anInt)
        {
            return anInt;
        }

        public static bool toBool(this int anInt)
        {
            return anInt > 0f;
        }

        public static float toFloat(this int anInt)
        {
            return (float)anInt;
        }

    }

}
