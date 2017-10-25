using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

namespace ExifLibrary
{
    public static class ExifExtensionMethods
    {
        /// <summary>
        /// Reads count bytes from the current stream into a byte array and advances
        /// the current position by count bytes.
        /// </summary>
        /// <param name="count">The number of bytes to read.</param>
        /// <returns>
        /// A byte array of given size read from the stream, or null
        /// if end of file is reached before reading count bytes.
        /// </returns>
        public static byte[] ReadBytes(FileStream stream, int count)
        {
            byte[] buffer = new byte[count];
            int offset = 0;
            int remaining = count;
            while (remaining > 0)
            {
                int read = stream.Read(buffer, offset, remaining);
                if (read <= 0)
                    return null;
                remaining -= read;
                offset += read;
            }
            return buffer;
        }

        /// <summary>
        /// Writes the given byte array to the current stream and advances
        ///  the current position by the length of the array.
        /// </summary>
        /// <param name="buffer">A byte array containing the data to write.</param>
        public static void WriteBytes(FileStream stream, byte[] buffer)
        {
            stream.Write(buffer, 0, buffer.Length);
        }
    }
}
