using UnityEngine;

using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;

using Nanome.Core;

namespace Nanome.Core.Extension
{

    public static class StringExtension
    {

        public static string SubstringTrim(this string str, int idx, int size)
        {
            var l = str.Length;
            while (size > 0 && idx < l)
            {
                var c = str[idx];
                if (c == ' ' || c == '\t' || c == '\n' || c == '\r')
                {
                    idx++;
                    size--;
                }
                else
                {
                    break;
                }
            }
            var end = idx + size - 1;
            while (size > 0 && end >= 0)
            {
                var c = str[end];
                if (c == ' ' || c == '\t' || c == '\n' || c == '\r')
                {
                    end--;
                    size--;
                }
                else
                {
                    break;
                }
            }
            return str.Substring(idx, size);
        }

        public static bool ContainsFast(this string str, string compared)
        {
            return str.IndexOf(compared, StringComparison.Ordinal) >= 0;
        }

        private static unsafe void FastToLower(char* input, int len)
        {
            char cmin = 'A';
            char cmax = 'Z';
            char cdiff = (char)('a' - 'A');
            for (int i = 0; i < len; i++)
            {
                char c = input[i];
                if (c >= cmin && c <= cmax)
                {
                    input[i] = (char)(c + cdiff);
                }
            }
        }

        public static string FastToLower(this string str)
        {
            string result = string.Copy(str);
            unsafe
            {
                fixed (char* p = result)
                {
                    FastToLower(p, result.Length);
                }
            }
            return result;
        }

        public static void AppendNewLine(this StringBuilder sb)
        {
            sb.Append(Environment.NewLine);
        }
        public static void AppendNewLine(this StringBuilder sb, string str)
        {
            sb.Append(str);
            sb.Append(Environment.NewLine);
        }

        public static void AppendPadRight(this StringBuilder sb, int pad, string str)
        {
            sb.Append(str);
            var diff = pad - str.Length;
            if (diff > 0)
            {
                sb.Append(' ', diff);
            }
        }

        public static void AppendPadLeft(this StringBuilder sb, int pad, string str)
        {
            var diff = pad - str.Length;
            if (diff > 0)
            {
                sb.Append(' ', diff);
            }
            sb.Append(str);
        }

        public static void AppendPadLeftInt(this StringBuilder sb, int pad, int value)
        {
            var negative = value < 0;
            var abs = value;
            if (negative)
            {
                abs = -value;
            }
            var chars = ThreadLocalBuffers.chars();
            var idx = 31;
            var size = 0;
            while (abs > 0 || size == 0)
            {
                chars[idx] = (char)((byte)(abs % 10) + (byte)'0');
                abs /= 10;
                idx--;
                size++;
            }
            if (negative)
            {
                chars[idx] = '-';
                idx--;
                size++;
            }
            var diff = pad - size;
            if (diff > 0)
            {
                sb.Append(' ', diff);
            }
            sb.Append(chars, idx + 1, size);
        }

        public static void AppendPadLeftFloat(this StringBuilder sb, int pad, float value, int digits)
        {
            var negative = value < 0;
            var abs = value;
            if (negative)
            {
                abs = -value;
            }
            var chars = ThreadLocalBuffers.chars();
            var idx = 15;
            var size = 0;
            var integer = (int)abs;
            while (integer > 0 || size == 0)
            {
                chars[idx] = (char)((byte)(integer % 10) + (byte)'0');
                integer /= 10;
                idx--;
                size++;
            }
            if (negative)
            {
                chars[idx] = '-';
                idx--;
                size++;
            }
            if (digits > 0)
            {
                size += digits + 1;
                var idx2 = 16;
                chars[idx2] = '.';
                idx2 += digits;
                var deci = (int)Math.Round(((abs - Math.Floor(abs)) * Math.Pow(10f, digits)));
                for (var i = 0; i < digits; i++)
                {
                    chars[idx2] = (char)((byte)(deci % 10) + (byte)'0');
                    deci /= 10;
                    idx2--;
                }
            }
            var diff = pad - size;
            if (diff > 0)
            {
                sb.Append(' ', diff);
            }
            sb.Append(chars, idx + 1, size);
        }

        public static List<string> SplitInParts(this String s, Int32 partLength)
        {
            if (s == null)
            {
                throw new ArgumentNullException("s");
            }
            if (partLength <= 0)
            {
                throw new ArgumentException("Part length has to be positive.", "partLength");
            }
            var parts = new List<string>();
            for (var i = 0; i < s.Length; i += partLength)
            {
                parts.Add(s.Substring(i, Math.Min(partLength, s.Length - i)));
            }
            return parts;
        }

        public static string toString(this string aStr)
        {
            if (aStr == null)
            {
                return "";
            }
            return aStr;
        }

        public static int toInt(this string str)
        {
            int parsedInt = 0;
            if (str != null && int.TryParse(str, out parsedInt))
            {
                return parsedInt;
            }
            else
            {
                return 0;
            }
        }

        public static float toFloat(this string aStr)
        {
            float parsedFloat = 0.0f;
            if (aStr != null && float.TryParse(aStr, out parsedFloat))
            {
                return parsedFloat;
            }
            else
            {
                return 0.0f;
            }
        }

        public static double toDouble(this string aStr)
        {
            double parsed = 0.0;
            if (aStr != null && double.TryParse(aStr, out parsed))
            {
                return parsed;
            }
            else
            {
                return 0.0;
            }
        }

        public static bool toBool(this string aStr)
        {
            int parsedInt = 0;
            if (aStr != null && aStr.ToLower() == "true")
            {
                return true;
            }
            if (aStr != null && aStr.ToLower() == "false")
            {
                return false;
            }
            if (aStr != null && int.TryParse(aStr, out parsedInt))
            {
                return (parsedInt > 0);
            }
            else
            {
                return false;
            }
        }

        public static Color toColor(this string aStr)
        {
            Color clr = new Color(0, 0, 0, 0);
            if (aStr != null && aStr.Length > 0)
            {
                try
                {
                    if (aStr.Substring(0, 1) == "#" || aStr.Length == 6) // #FFFFFF / FFFFFF format
                    {
                        var str = aStr;
                        if (aStr.Length > 6)
                        {
                            str = aStr.Substring(1, aStr.Length - 1);
                        }
                        try
                        {
                            clr.r = (float)System.Int32.Parse(str.Substring(0, 2), NumberStyles.AllowHexSpecifier) / 255.0f;
                            clr.g = (float)System.Int32.Parse(str.Substring(2, 2), NumberStyles.AllowHexSpecifier) / 255.0f;
                            clr.b = (float)System.Int32.Parse(str.Substring(4, 2), NumberStyles.AllowHexSpecifier) / 255.0f;
                        }
                        catch (Exception e)
                        {
                            Debug.Log("Could not convert " + aStr + " to Color. " + e);
                        }
                        if (str.Length == 8)
                        {
                            clr.a = System.Int32.Parse(str.Substring(6, 2), NumberStyles.AllowHexSpecifier) / 255.0f;
                        }
                        else
                        {
                            clr.a = 1.0f;
                        }
                    }
                    else if (aStr.IndexOf(",", 0) >= 0) // 0.3, 1.0, 0.2 format
                    {
                        int p0 = 0;
                        int p1 = 0;
                        int c = 0;
                        float divideBy = 1f;
                        if (aStr.IndexOf(".", 0) < 0)
                        {
                            divideBy = 255f;
                        }
                        p1 = aStr.IndexOf(",", p0);
                        while (p1 > p0 && c < 4)
                        {
                            clr[c++] = Mathf.Clamp01(aStr.Substring(p0, p1 - p0).toFloat() / divideBy);
                            p0 = p1 + 1;
                            if (p0 < aStr.Length)
                            {
                                p1 = aStr.IndexOf(",", p0);
                            }
                            if (p1 < 0)
                            {
                                p1 = aStr.Length;
                            }
                        }
                        if (c < 4)
                        {
                            clr.a = 1.0f;
                        }
                    }
                    else if (aStr.IndexOf(" ", 0) >= 0) // 0.3 1.0 0.2 format
                    {
                        int p0 = 0;
                        int p1 = 0;
                        int c = 0;
                        float divideBy = 1f;
                        if (aStr.IndexOf(".", 0) < 0)
                        {
                            divideBy = 255f;
                        }
                        p1 = aStr.IndexOf(" ", p0);
                        while (p1 > p0 && c < 4)
                        {
                            clr[c++] = Mathf.Clamp01(aStr.Substring(p0, p1 - p0).toFloat() / divideBy);
                            p0 = p1 + 1;
                            if (p0 < aStr.Length)
                            {
                                p1 = aStr.IndexOf(" ", p0);
                            }
                            if (p1 < 0)
                            {
                                p1 = aStr.Length;
                            }
                        }
                        if (c < 4)
                        {
                            clr.a = 1.0f;
                        }
                    }
                }
                catch (Exception e)
                {
                    Debug.Log("Could not convert " + aStr + " to Color. " + e);
                }
            }
            return clr;
        }

        public static Vector3 MakeVector3(this string aStr)
        {
            Vector3 v = new Vector3(0, 0, 0);
            if (aStr != null && aStr.Length > 0)
            {
                try
                {
                    if (aStr.IndexOf(",", 0) >= 0)
                    {  // 0.3, 1.0, 0.2 format
                        int p0 = 0;
                        int p1 = 0;
                        int c = 0;
                        p1 = aStr.IndexOf(",", p0);
                        while (p1 > p0 && c <= 3)
                        {
                            v[c++] = float.Parse(aStr.Substring(p0, p1 - p0));
                            p0 = p1 + 1;
                            if (p0 < aStr.Length) p1 = aStr.IndexOf(",", p0);
                            if (p1 < 0) p1 = aStr.Length;
                        }
                    }
                }
                catch (Exception e)
                {
                    Debug.Log("Could not convert " + aStr + " to Vector3. " + e);
                    return new Vector3(0, 0, 0);
                }
            }
            return v;
        }

        public static Vector4 MakeVector4(this string aStr)
        {
            Vector4 v = new Vector4(0, 0, 0, 0);
            if (aStr != null && aStr.Length > 0)
            {
                try
                {
                    if (aStr.IndexOf(",", 0) >= 0)
                    {  // 0.3, 1.0, 0.2 format
                        int p0 = 0;
                        int p1 = 0;
                        int c = 0;
                        p1 = aStr.IndexOf(",", p0);
                        while (p1 > p0 && c <= 4)
                        {
                            v[c++] = float.Parse(aStr.Substring(p0, p1 - p0));
                            p0 = p1 + 1;
                            if (p0 < aStr.Length) p1 = aStr.IndexOf(",", p0);
                            if (p1 < 0) p1 = aStr.Length;
                        }
                    }
                }
                catch (Exception e)
                {
                    Debug.Log("Could not convert " + aStr + " to Vector3. " + e);
                    return new Vector4(0, 0, 0, 0);
                }
            }
            return v;
        }

        public static int CountChars(this string str, string searchChars)
        {
            int tot = 0;
            for (int i = 0; i < str.Length; i++)
            {
                char c = str[i];
                if (searchChars.IndexOf(c) >= 0) tot++;
            }
            return tot;
        }

        public static int IndexOfChars(this string str, string searchChars, int startAt)
        {
            for (int i = startAt; i < str.Length; i++)
            {
                char c = str[i];
                if (searchChars.IndexOf(c) >= 0) return i;
            }
            return -1;
        }

        public static int IndexOfEndOfLine(this string str, int startAt)
        {
            int i = str.IndexOf('\n', startAt);
            if (i >= startAt)
            {
                if (i > 0 && str[i - 1] == '\r') return i - 1;
                return i;
            }
            return str.IndexOf('\r', startAt);
        }

        public static int EndOfCharRepetition(this string str, int startAt)
        {
            if (startAt < str.Length)
            {
                int i = startAt;
                char c = str[i];
                while (i < str.Length - 1)
                {
                    i++;
                    if (str[i] != c) return i;
                }
            }
            return str.Length;
        }

        public static string Truncate(this string str, int maxLength)
        {
            if (str.Length > maxLength) return str.Substring(0, maxLength);
            return str;
        }

        public static string TrimChars(this string str, string trimChars)
        {
            int b = 0;
            int e = str.Length;
            for (; b < e; b++)
            {
                char c = str[b];
                if (trimChars.IndexOf(c) < 0) break;
            }

            for (; e > b; e--)
            {
                char c = str[e];
                if (trimChars.IndexOf(c) < 0) break;
            }
            if (b > 0 || e < str.Length) return str.Substring(b, e - b);
            return str;
        }

        public static string SubstringAfter(this string str, string after)
        {
            int pos = str.IndexOf(after);
            if (pos >= 0)
            {
                pos += after.Length;
                return str.Substring(pos, str.Length - pos);
            }
            return str;
        }

        public static string Right(this string str, int l)
        {
            int pos = str.Length - l;
            if (pos >= 0)
            {
                return str.Substring(pos, str.Length - pos);
            }
            return str;
        }

        private static unsafe int FastParseInt(char* input, int pos, int len)
        {
            int part = 0;          // the current part (int, float and sci parts of the number)
            bool neg = false;      // true if part is a negative number

            int* ret = stackalloc int[1];

            while (pos < len && (*(input + pos) > '9' || *(input + pos) < '0') && *(input + pos) != '-')
                pos++;

            // sign
            if (*(input + pos) == '-')
            {
                neg = true;
                pos++;
            }

            // integer part
            while (pos < len && !(input[pos] > '9' || input[pos] < '0'))
                part = part * 10 + (input[pos++] - '0');

            *ret = neg ? (part * -1) : part;
            return *ret;
        }

        private static unsafe long FastParseLong(char* input, int len)
        {
            int pos = 0;
            long part = 0;
            bool neg = false;

            long* ret = stackalloc long[1];

            while (pos < len && (*(input + pos) > '9' || *(input + pos) < '0') && *(input + pos) != '-')
                pos++;

            // sign
            if (*(input + pos) == '-')
            {
                neg = true;
                pos++;
            }

            // long part
            while (pos < len && !(input[pos] > '9' || input[pos] < '0'))
                part = part * 10 + (input[pos++] - '0');

            *ret = neg ? (part * -1) : part;
            return *ret;
        }

        public static int FastParseInt(this string str)
        {
            // THIS IS CUSTOM DESIGNED FAST PARSING, unsafe code but very usefull for large computations
            int v;
            unsafe
            {
                fixed (char* p = str)
                {
                    v = FastParseInt(p, 0, str.Length);
                }
            }
            return v;
        }

        public static int FastParseInt(this string str, int start, int length)
        {
            // THIS IS CUSTOM DESIGNED FAST PARSING, unsafe code but very usefull for large computations
            int v;
            unsafe
            {
                fixed (char* p = str)
                {
                    v = FastParseInt(p, start, start + length);
                }
            }
            return v;
        }

        public static long FastParseLong(this string str)
        {
            // THIS IS CUSTOM DESIGNED FAST PARSING, unsafe code but very usefull for large computations
            long v;
            unsafe
            {
                fixed (char* p = str)
                {
                    v = FastParseLong(p, str.Length);
                }
            }
            return v;
        }

        public static string[] Split(this string str, string sep, bool allowEmpty = true, int maxSplits = 999999999)
        {
            if (!allowEmpty)
            {
                return str.Split(new string[] { sep }, maxSplits + 1, StringSplitOptions.RemoveEmptyEntries);
            }
            return str.Split(new string[] { sep }, maxSplits + 1, StringSplitOptions.None);
        }
        public static string[] Split(this string str, string sep1, string sep2, bool allowEmpty = true, int maxSplits = 999999999)
        {
            if (!allowEmpty)
            {
                return str.Split(new string[] { sep1, sep2 }, maxSplits + 1, StringSplitOptions.RemoveEmptyEntries);
            }
            return str.Split(new string[] { sep1, sep2 }, maxSplits + 1, StringSplitOptions.None);
        }
        public static string[] Split(this string str, string sep1, string sep2, string sep3, bool allowEmpty = true, int maxSplits = 999999999)
        {
            if (!allowEmpty)
            {
                return str.Split(new string[] { sep1, sep2, sep3 }, maxSplits + 1, StringSplitOptions.RemoveEmptyEntries);
            }
            return str.Split(new string[] { sep1, sep2, sep3 }, maxSplits + 1, StringSplitOptions.None);
        }

        private static unsafe float FastParseFloat(char* input, int pos, int len)
        {
            int part = 0;          // the current part (int, float and sci parts of the number)
            bool neg = false;      // true if part is a negative number

            float* ret = stackalloc float[1];

            // find start
            while (pos < len && (input[pos] < '0' || input[pos] > '9') && input[pos] != '-' && input[pos] != '.')
                pos++;

            // sign
            if (input[pos] == '-')
            {
                neg = true;
                pos++;
            }

            // integer part
            while (pos < len && !(input[pos] > '9' || input[pos] < '0'))
                part = part * 10 + (input[pos++] - '0');

            *ret = neg ? (float)(part * -1) : (float)part;

            // float part
            if (pos < len && input[pos] == '.')
            {
                pos++;
                double mul = 1;
                part = 0;

                while (pos < len && !(input[pos] > '9' || input[pos] < '0'))
                {
                    part = part * 10 + (input[pos] - '0');
                    mul *= 10;
                    pos++;
                }

                if (neg)
                    *ret -= (float)part / (float)mul;
                else
                    *ret += (float)part / (float)mul;

            }

            // scientific part
            if (pos < len && (input[pos] == 'e' || input[pos] == 'E'))
            {
                pos++;
                neg = (input[pos] == '-'); pos++;
                part = 0;
                while (pos < len && !(input[pos] > '9' || input[pos] < '0'))
                {
                    part = part * 10 + (input[pos++] - '0');
                }

                if (neg)
                    *ret /= (float)Math.Pow(10d, (double)part);
                else
                    *ret *= (float)Math.Pow(10d, (double)part);
            }

            return (float)*ret;
        }

        public static float FastParseFloat(this string str)
        {
            // THIS IS CUSTOM DESIGNED FAST PARSING, unsafe code but very usefull for large computations
            float v;
            unsafe
            {
                fixed (char* p = str)
                {
                    v = FastParseFloat(p, 0, str.Length);
                }
            }
            return v;
        }

        public static float FastParseFloat(this string str, int start, int length)
        {
            // THIS IS CUSTOM DESIGNED FAST PARSING, unsafe code but very usefull for large computations
            float v;
            unsafe
            {
                fixed (char* p = str)
                {
                    v = FastParseFloat(p, start, start + length);
                }
            }
            return v;
        }

        public static string XmlDecode(this string str)
        {
            str = str.Replace("&lt;", "<");
            str = str.Replace("&gt;", ">");
            str = str.Replace("&amp;", "&");
            str = str.Replace("&apos;", "'");
            str = str.Replace("&quot;", "\"");
            return str;
        }

        public static string XmlEncode(this string str)
        {
            str = str.Replace("&", "&amp;");
            str = str.Replace("<", "&lt;");
            str = str.Replace(">", "&gt;");
            str = str.Replace("'", "&apos;");
            str = str.Replace("\"", "&quot;");
            return str;
        }

        public static string JsonDecode(this string str)
        {
            str = str.Replace("\\/", "/");
            str = str.Replace("\\n", "\n");
            str = str.Replace("\\r", "\r");
            str = str.Replace("\\t", "\t");
            str = str.Replace("\\\"", "\"");
            str = str.Replace("\\\\", "\\");
            return str;
        }

        public static string JsonEncode(this string str)
        {
            str = str.Replace("\"", "\\\"");
            str = str.Replace("\\", "\\\\");
            str = str.Replace("/", "\\/");
            str = str.Replace("\n", "\\n");
            str = str.Replace("\r", "\\r");
            str = str.Replace("\t", "\\t");
            return str;
        }

    }
}