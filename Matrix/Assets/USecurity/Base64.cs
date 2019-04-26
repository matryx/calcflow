using System;
using System.Text;

namespace Assets.USecurity
{
    /// <summary>
    /// Base64 helper.
    /// </summary>
	public static class Base64
    {
        /// <summary>
        /// Encode plain string to Base64.
        /// </summary>
        public static string Encode(string plainText)
        {
            return Convert.ToBase64String(Encoding.UTF8.GetBytes(plainText));
        }

        /// <summary>
        /// Decode encoded Base64-string.
        /// </summary>
        public static string Decode(string base64EncodedString)
        {
            return Encoding.UTF8.GetString(Convert.FromBase64String(base64EncodedString));
        }
    }
}