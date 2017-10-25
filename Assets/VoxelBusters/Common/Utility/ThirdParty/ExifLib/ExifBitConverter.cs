#if !UNITY_WINRT
using System;
using System.Text;

namespace ExifLibrary
{
    /// <summary>
    /// Converts between exif data types and array of bytes.
    /// </summary>
    public class ExifBitConverter : BitConverterEx
    {
        #region "Constructors"
        public ExifBitConverter(ByteOrder from, ByteOrder to)
            : base(from, to)
        {
            ;
        }
        #endregion

        #region "Static Methods"
        /// <summary>
        /// Returns an ASCII string converted from the given byte array.
        /// </summary>
        public static string ToAscii(byte[] data, bool endatfirstnull)
        {
            int len = data.Length;
            if (endatfirstnull)
            {
                len = Array.IndexOf(data, (byte)0);
                if (len == -1) len = data.Length;
            }
            return Encoding.UTF8.GetString(data, 0, len);
        }

        /// <summary>
        /// Returns an ASCII string converted from the given byte array.
        /// </summary>
        public static string ToAscii(byte[] data)
        {
            return ToAscii(data, true);
        }

        /// <summary>
        /// Returns a string converted from the given byte array.
        /// from the numeric value of each byte.
        /// </summary>
        public static string ToString(byte[] data)
        {
            StringBuilder sb = new StringBuilder();
            foreach (byte b in data)
                sb.Append(b);
            return sb.ToString();
        }

        /// <summary>
        /// Returns a DateTime object converted from the given byte array.
        /// </summary>
        public static DateTime ToDateTime(byte[] data, bool hastime)
        {
            string str = ToAscii(data);
            if (hastime)
                return DateTime.ParseExact(str, "yyyy:MM:dd HH:mm:ss", System.Globalization.CultureInfo.InvariantCulture);
            else
                return DateTime.ParseExact(str, "yyyy:MM:dd", System.Globalization.CultureInfo.InvariantCulture);
        }

        /// <summary>
        /// Returns a DateTime object converted from the given byte array.
        /// </summary>
        public static DateTime ToDateTime(byte[] data)
        {
            return ToDateTime(data, true);
        }

        /// <summary>
        /// Returns an unsigned rational number converted from the first 
        /// eight bytes of the given byte array. The first four bytes are
        /// assumed to be the numerator and the next four bytes are the
        /// denumerator.
        /// Numbers are converted from the given byte-order to platform byte-order.
        /// </summary>
        public static MathEx.UFraction32 ToURational(byte[] data, ByteOrder frombyteorder)
        {
            byte[] num = new byte[4];
            byte[] den = new byte[4];
            Array.Copy(data, 0, num, 0, 4);
            Array.Copy(data, 4, den, 0, 4);
            return new MathEx.UFraction32(ToUInt32(num, 0, frombyteorder, ByteOrder.System), ToUInt32(den, 0, frombyteorder, ByteOrder.System));
        }

        /// <summary>
        /// Returns a signed rational number converted from the first 
        /// eight bytes of the given byte array. The first four bytes are
        /// assumed to be the numerator and the next four bytes are the
        /// denumerator.
        /// Numbers are converted from the given byte-order to platform byte-order.
        /// </summary>
        public static MathEx.Fraction32 ToSRational(byte[] data, ByteOrder frombyteorder)
        {
            byte[] num = new byte[4];
            byte[] den = new byte[4];
            Array.Copy(data, 0, num, 0, 4);
            Array.Copy(data, 4, den, 0, 4);
            return new MathEx.Fraction32(ToInt32(num, 0, frombyteorder, ByteOrder.System), ToInt32(den, 0, frombyteorder, ByteOrder.System));
        }

        /// <summary>
        /// Returns an array of 16-bit unsigned integers converted from 
        /// the given byte array.
        /// Numbers are converted from the given byte-order to platform byte-order.
        /// </summary>
        public static ushort[] ToUShortArray(byte[] data, int count, ByteOrder frombyteorder)
        {
            ushort[] numbers = new ushort[count];
            for (uint i = 0; i < count; i++)
            {
                byte[] num = new byte[2];
                Array.Copy(data, i * 2, num, 0, 2);
                numbers[i] = ToUInt16(num, 0, frombyteorder, ByteOrder.System);
            }
            return numbers;
        }

        /// <summary>
        /// Returns an array of 32-bit unsigned integers converted from 
        /// the given byte array.
        /// Numbers are converted from the given byte-order to platform byte-order.
        /// </summary>
        public static uint[] ToUIntArray(byte[] data, int count, ByteOrder frombyteorder)
        {
            uint[] numbers = new uint[count];
            for (uint i = 0; i < count; i++)
            {
                byte[] num = new byte[4];
                Array.Copy(data, i * 4, num, 0, 4);
                numbers[i] = ToUInt32(num, 0, frombyteorder, ByteOrder.System);
            }
            return numbers;
        }

        /// <summary>
        /// Returns an array of 32-bit signed integers converted from 
        /// the given byte array.
        /// Numbers are converted from the given byte-order to platform byte-order.
        /// </summary>
        public static int[] ToSIntArray(byte[] data, int count, ByteOrder byteorder)
        {
            int[] numbers = new int[count];
            for (uint i = 0; i < count; i++)
            {
                byte[] num = new byte[4];
                Array.Copy(data, i * 4, num, 0, 4);
                numbers[i] = ToInt32(num, 0, byteorder, ByteOrder.System);
            }
            return numbers;
        }

        /// <summary>
        /// Returns an array of unsigned rational numbers converted from 
        /// the given byte array.
        /// Numbers are converted from the given byte-order to platform byte-order.
        /// </summary>
        public static MathEx.UFraction32[] ToURationalArray(byte[] data, int count, ByteOrder frombyteorder)
        {
            MathEx.UFraction32[] numbers = new MathEx.UFraction32[count];
            for (uint i = 0; i < count; i++)
            {
                byte[] num = new byte[4];
                byte[] den = new byte[4];
                Array.Copy(data, i * 8, num, 0, 4);
                Array.Copy(data, i * 8 + 4, den, 0, 4);
                numbers[i].Set(ToUInt32(num, 0, frombyteorder, ByteOrder.System), ToUInt32(den, 0, frombyteorder, ByteOrder.System));
            }
            return numbers;
        }

        /// <summary>
        /// Returns an array of signed rational numbers converted from 
        /// the given byte array.
        /// Numbers are converted from the given byte-order to platform byte-order.
        /// </summary>
        public static MathEx.Fraction32[] ToSRationalArray(byte[] data, int count, ByteOrder frombyteorder)
        {
            MathEx.Fraction32[] numbers = new MathEx.Fraction32[count];
            for (uint i = 0; i < count; i++)
            {
                byte[] num = new byte[4];
                byte[] den = new byte[4];
                Array.Copy(data, i * 8, num, 0, 4);
                Array.Copy(data, i * 8 + 4, den, 0, 4);
                numbers[i].Set(ToInt32(num, 0, frombyteorder, ByteOrder.System), ToInt32(den, 0, frombyteorder, ByteOrder.System));
            }
            return numbers;
        }

        /// <summary>
        /// Converts the given ascii string to an array of bytes optionally adding a null terminator.
        /// </summary>
        public static byte[] GetBytes(string value, bool addnull)
        {
            if (addnull) value += '\0';
            return Encoding.UTF8.GetBytes(value);
        }

        /// <summary>
        /// Converts the given ascii string to an array of bytes without adding a null terminator.
        /// </summary>
        public static byte[] GetBytes(string value)
        {
            return GetBytes(value, false);
        }

        /// <summary>
        /// Converts the given datetime to an array of bytes with a null terminator.
        /// </summary>
        public static byte[] GetBytes(DateTime value, bool hastime)
        {
            string str = "";
            if (hastime)
                str = value.ToString("yyyy:MM:dd HH:mm:ss", System.Globalization.CultureInfo.InvariantCulture);
            else
                str = value.ToString("yyyy:MM:dd", System.Globalization.CultureInfo.InvariantCulture);
            return GetBytes(str, true);
        }

        /// <summary>
        /// Converts the given unsigned rational number to an array of bytes.
        /// Numbers are converted from the platform byte-order to the given byte-order.
        /// </summary>
        public static byte[] GetBytes(MathEx.UFraction32 value, ByteOrder tobyteorder)
        {
            byte[] num = GetBytes(value.Numerator, ByteOrder.System, tobyteorder);
            byte[] den = GetBytes(value.Denominator, ByteOrder.System, tobyteorder);
            byte[] data = new byte[8];
            Array.Copy(num, 0, data, 0, 4);
            Array.Copy(den, 0, data, 4, 4);
            return data;
        }

        /// <summary>
        /// Converts the given signed rational number to an array of bytes.
        /// Numbers are converted from the platform byte-order to the given byte-order.
        /// </summary>
        public static byte[] GetBytes(MathEx.Fraction32 value, ByteOrder tobyteorder)
        {
            byte[] num = GetBytes(value.Numerator, ByteOrder.System, tobyteorder);
            byte[] den = GetBytes(value.Denominator, ByteOrder.System, tobyteorder);
            byte[] data = new byte[8];
            Array.Copy(num, 0, data, 0, 4);
            Array.Copy(den, 0, data, 4, 4);
            return data;
        }

        /// <summary>
        /// Converts the given array of 16-bit unsigned integers to an array of bytes.
        /// Numbers are converted from the platform byte-order to the given byte-order.
        /// </summary>
        public static byte[] GetBytes(ushort[] value, ByteOrder tobyteorder)
        {
            byte[] data = new byte[2 * value.Length];
            for (int i = 0; i < value.Length; i++)
            {
                byte[] num = GetBytes(value[i], ByteOrder.System, tobyteorder);
                Array.Copy(num, 0, data, i * 2, 2);
            }
            return data;
        }

        /// <summary>
        /// Converts the given array of 32-bit unsigned integers to an array of bytes.
        /// Numbers are converted from the platform byte-order to the given byte-order.
        /// </summary>
        public static byte[] GetBytes(uint[] value, ByteOrder tobyteorder)
        {
            byte[] data = new byte[4 * value.Length];
            for (int i = 0; i < value.Length; i++)
            {
                byte[] num = GetBytes(value[i], ByteOrder.System, tobyteorder);
                Array.Copy(num, 0, data, i * 4, 4);
            }
            return data;
        }

        /// <summary>
        /// Converts the given array of 32-bit signed integers to an array of bytes.
        /// Numbers are converted from the platform byte-order to the given byte-order.
        /// </summary>
        public static byte[] GetBytes(int[] value, ByteOrder tobyteorder)
        {
            byte[] data = new byte[4 * value.Length];
            for (int i = 0; i < value.Length; i++)
            {
                byte[] num = GetBytes(value[i], ByteOrder.System, tobyteorder);
                Array.Copy(num, 0, data, i * 4, 4);
            }
            return data;
        }

        /// <summary>
        /// Converts the given array of unsigned rationals to an array of bytes.
        /// Numbers are converted from the platform byte-order to the given byte-order.
        /// </summary>
        public static byte[] GetBytes(MathEx.UFraction32[] value, ByteOrder tobyteorder)
        {
            byte[] data = new byte[8 * value.Length];
            for (int i = 0; i < value.Length; i++)
            {
                byte[] num = GetBytes(value[i].Numerator, ByteOrder.System, tobyteorder);
                byte[] den = GetBytes(value[i].Denominator, ByteOrder.System, tobyteorder);
                Array.Copy(num, 0, data, i * 8, 4);
                Array.Copy(den, 0, data, i * 8 + 4, 4);
            }
            return data;
        }

        /// <summary>
        /// Converts the given array of signed rationals to an array of bytes.
        /// Numbers are converted from the platform byte-order to the given byte-order.
        /// </summary>
        public static byte[] GetBytes(MathEx.Fraction32[] value, ByteOrder tobyteorder)
        {
            byte[] data = new byte[8 * value.Length];
            for (int i = 0; i < value.Length; i++)
            {
                byte[] num = GetBytes(value[i].Numerator, ByteOrder.System, tobyteorder);
                byte[] den = GetBytes(value[i].Denominator, ByteOrder.System, tobyteorder);
                Array.Copy(num, 0, data, i * 8, 4);
                Array.Copy(den, 0, data, i * 8 + 4, 4);
            }
            return data;
        }
        #endregion
    }
}
#endif