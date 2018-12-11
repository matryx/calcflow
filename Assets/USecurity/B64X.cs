using System;
using System.Text;

namespace Assets.USecurity
{
    /// <summary>
    /// Simple and fast Base64 XOR encoding with a key. Suitable for data protection in RAM. Do NOT use for secure data encryption.
    /// </summary>
    public class B64X
    {
        /// <summary>
        /// Encrypt plain string.
        /// </summary>
        public static string Encrypt(string value, string key)
        {
            return Convert.ToBase64String(Encode(Encoding.UTF8.GetBytes(value), Encoding.UTF8.GetBytes(key)));
        }

        /// <summary>
        /// Decrypt encrypted string.
        /// </summary>
        public static string Decrypt(string value, string key)
        {
            return Encoding.UTF8.GetString(Encode(Convert.FromBase64String(value), Encoding.UTF8.GetBytes(key)));
        }

        private static byte[] Encode(byte[] bytes, byte[] key)
        {
            var j = 0;

            for (var i = 0; i < bytes.Length; i++)
            {
                bytes[i] ^= key[j];

                if (++j == key.Length)
                {
                    j = 0;
                }
            }

            return bytes;
        }
    }
}