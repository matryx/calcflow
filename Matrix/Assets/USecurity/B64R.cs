using System.Collections.Generic;

namespace Assets.USecurity
{
    /// <summary>
    /// Simple and fast Base64 encoding algorithm with byte reverse. Suitable for data protection in RAM. Use for unsafe data storing outside RAM. Do NOT use for secure data encryption.
    /// </summary>
    public class B64R
    {
        /// <summary>
        /// Encode plain string
        /// </summary>
        public static string Encode(string value)
        {
            var base64 = Base64.Encode(value);
            var chars = base64.ToCharArray();

            Reverse(chars);

            return new string(chars);
        }

        /// <summary>
        /// Decode protected string.
        /// </summary>
        public static string Decode(string value)
        {
            var chars = value.ToCharArray();

            Reverse(chars);

            return Base64.Decode(new string(chars));
        }

        private static void Reverse(IList<char> chars)
        {
            for (var i = 1; i < chars.Count; i += 2)
            {
                var c = chars[i];

                chars[i] = chars[i - 1];
                chars[i - 1] = c;
            }
        }
    }
}