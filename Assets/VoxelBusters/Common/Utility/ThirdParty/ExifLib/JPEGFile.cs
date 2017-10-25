using System;
using System.Collections.Generic;
using System.Text;
//using System.Drawing;
using System.IO;
using System.ComponentModel;

namespace ExifLibrary
{
    /// <summary>
    /// Represents the binary view of a JPEG file.
    /// </summary>
    public class JPEGFile
    {
        #region "Properties"
        /// <summary>
        /// Gets or sets the sections contained in the JPEG file.
        /// </summary>
        public List<JPEGSection> Sections { get; set; }
        /// <summary>
        /// Gets or sets non-standard trailing data following the End of Image (EOI) marker.
        /// </summary>
        public byte[] TrailingData { get; set; }
        #endregion

        #region "Constructors"
        public JPEGFile()
        {
            ;
        }

        /// <summary>
        /// Constructs a JPEGFile class by reading the given stream.
        /// </summary>
        /// <param name="stream">The data stream used to load the image.</param>
        /// <exception cref="NotValidJPEGFileException"></exception>
        public JPEGFile(Stream stream)
        {
            Read(stream);
        }

        /// <summary>
        /// Constructs a JPEGFile class by reading the given JPEG file.
        /// </summary>
        /// <param name="filename">The complete path to the JPEG file.</param>
        /// <exception cref="NotValidJPEGFileException"></exception>
        public JPEGFile(string filename)
        {
            using (FileStream stream = new FileStream(filename, FileMode.Open, FileAccess.Read))
            {
                Read(stream);
            }
        }
        #endregion

        #region "Instance Methods"
        /// <summary>
        /// Saves the JPEG image to the given stream. The caller is responsible for
        /// disposing the stream.
        /// </summary>
        /// <param name="stream">The data stream used to save the image.</param>
        public void Save(Stream stream)
        {
            // Write sections
            foreach (JPEGSection section in Sections)
            {
                // Section header (including length bytes and section marker) 
                // must not exceed 64 kB.
                if (section.Header.Length + 2 + 2 > 64 * 1024)
                    throw new SectionExceeds64KBException();

                // Write section marker
                stream.Write(new byte[] { 0xFF, (byte)section.Marker }, 0, 2);

                // Write section header
                if (section.Header.Length != 0)
                {
                    // Header length including the length field itself
                    stream.Write(BitConverterEx.BigEndian.GetBytes((ushort)(section.Header.Length + 2)), 0, 2);

                    // Section header
                    stream.Write(section.Header, 0, section.Header.Length);
                }

                // Write entropy coded data
                if (section.EntropyData.Length != 0)
                    stream.Write(section.EntropyData, 0, section.EntropyData.Length);
            }

            // Write trailing data, if any
            if (TrailingData.Length != 0)
                stream.Write(TrailingData, 0, TrailingData.Length);
        }

        /// <summary>
        /// Saves the JPEG image with the given filename.
        /// </summary>
        /// <param name="filename">The complete path to the JPEG file.</param>
        public void Save(string filename)
        {
            using (FileStream stream = new FileStream(filename, FileMode.Create, FileAccess.Write))
            {
                Save(stream);
                stream.Close();
            }
        }

        /// <summary>
        /// Converts the JPEGFile to a System.Drawing.Bitmap.
        /// </summary>
        /// <returns>Returns a System.Drawing.Bitmap containing image data.</returns>
       /* public Bitmap ToBitmap()
        {
            Bitmap bmp;
            using (MemoryStream stream = new MemoryStream())
            {
                Save(stream);
                bmp = new Bitmap(stream);
                stream.Close();
            }
            return bmp;
        }
        */
        #endregion

        #region "Private Helper Methods"
        /// <summary>
        /// Reads the given stream.
        /// </summary>
        /// <param name="stream">The data stream used to load the image.</param>
        /// <exception cref="NotValidJPEGFileException"></exception>
        public bool Read(Stream stream)
        {
            Sections = new List<JPEGSection>();

            using (stream)
            {
                // Read the Start of Image (SOI) marker. SOI marker is represented
                // with two bytes: 0xFF, 0xD8.
                byte[] markerbytes = new byte[2];
                if (stream.Read(markerbytes, 0, 2) != 2 || markerbytes[0] != 0xFF && markerbytes[1] != 0xD8)
				{
					return false;
				}
                stream.Seek(0, SeekOrigin.Begin);

                // Search and read sections until we reach the end of file.
                while (stream.Position != stream.Length)
                {
                    // Read the next section marker. Section markers are two bytes 
                    // with values 0xFF, 0x?? where ?? must not be 0x00 or 0xFF.
                    if (stream.Read(markerbytes, 0, 2) != 2 || markerbytes[0] != 0xFF || markerbytes[1] == 0x00 || markerbytes[1] == 0xFF)
					{
						return false;
					}
                    JPEGMarker marker = (JPEGMarker)markerbytes[1];

                    byte[] header = new byte[0];
                    // SOI, EOI and RST markers do not contain any header
                    if (marker != JPEGMarker.SOI && marker != JPEGMarker.EOI && !(marker >= JPEGMarker.RST0 && marker <= JPEGMarker.RST7))
                    {
                        // Length of the header including the length bytes.
                        // This value is a 16-bit unsigned integer 
                        // in big endian byte-order.
                        byte[] lengthbytes = new byte[2];
                        if (stream.Read(lengthbytes, 0, 2) != 2)
						{
							return false;
						}
                        long length = (long)BitConverterEx.BigEndian.ToUInt16(lengthbytes, 0);

                        // Read section header.
                        header = new byte[length - 2];
                        int bytestoread = header.Length;
                        while (bytestoread > 0)
                        {
                            int count = Math.Min(bytestoread, 4 * 1024);
                            int bytesread = stream.Read(header, header.Length - bytestoread, count);
                            if (bytesread == 0)
							{
								return false;
							}
                            bytestoread -= bytesread;
                        }
                    }

                    // Start of Scan (SOS) sections and RST sections are immediately
                    // followed by entropy coded data. For that, we need to read until
                    // the next section marker once we reach a SOS or RST.
                    byte[] entropydata = new byte[0];
                    if (marker == JPEGMarker.SOS || (marker >= JPEGMarker.RST0 && marker <= JPEGMarker.RST7))
                    {
                        long position = stream.Position;

                        // Search for the next section marker
                        while (true)
                        {
                            // Search for an 0xFF indicating start of a marker
                            int nextbyte = 0;
                            do
                            {
                                nextbyte = stream.ReadByte();
                                if (nextbyte == -1)
								{
									return false;
								}
                            } while ((byte)nextbyte != 0xFF);

                            // Skip filler bytes (0xFF)
                            do
                            {
                                nextbyte = stream.ReadByte();
                                if (nextbyte == -1)
								{
									return false;
								}
                            } while ((byte)nextbyte == 0xFF);

                            // Looks like a section marker. The next byte must not be 0x00.
                            if ((byte)nextbyte != 0x00)
                            {
                                // We reached a section marker. Calculate the
                                // length of the entropy coded data.
                                stream.Seek(-2, SeekOrigin.Current);
                                long edlength = stream.Position - position;
                                stream.Seek(-edlength, SeekOrigin.Current);

                                // Read entropy coded data
                                entropydata = new byte[edlength];
                                int bytestoread = entropydata.Length;
                                while (bytestoread > 0)
                                {
                                    int count = Math.Min(bytestoread, 4 * 1024);
                                    int bytesread = stream.Read(entropydata, entropydata.Length - bytestoread, count);
                                    if (bytesread == 0)
									{
										return false;
									}
                                    bytestoread -= bytesread;
                                }

                                break;
                            }
                        }
                    }

                    // Store section.
                    JPEGSection section = new JPEGSection(marker, header, entropydata);
                    Sections.Add(section);

                    // Some propriety formats store data past the EOI marker
                    if (marker == JPEGMarker.EOI)
                    {
                        int bytestoread = (int)(stream.Length - stream.Position);
                        TrailingData = new byte[bytestoread];
                        while (bytestoread > 0)
                        {
                            int count = (int)Math.Min(bytestoread, 4 * 1024);
                            int bytesread = stream.Read(TrailingData, TrailingData.Length - bytestoread, count);
                            if (bytesread == 0)
							{
								return false;
							}
                            bytestoread -= bytesread;
                        }
                    }
                }
#if !NETFX_CORE
                stream.Close();
#else
				stream.Dispose();
#endif
            }
			return true;
        }
        #endregion
    }
}
