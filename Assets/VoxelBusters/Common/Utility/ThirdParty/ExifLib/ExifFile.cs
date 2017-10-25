#if !UNITY_WINRT
using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
//using System.Drawing;
using System.ComponentModel;

namespace ExifLibrary
{
    /// <summary>
    /// Reads and writes Exif data contained in a JPEG/Exif file.
    /// </summary>
    public class ExifFile
    {
        #region "Member Variables"
        private JPEGFile file;
        private JPEGSection app1;
        private uint makerNoteOffset;
        private long exifIFDFieldOffset, gpsIFDFieldOffset, interopIFDFieldOffset, firstIFDFieldOffset;
        private long thumbOffsetLocation, thumbSizeLocation;
        private uint thumbOffsetValue, thumbSizeValue;
        private bool makerNoteProcessed;
        #endregion

        #region "Properties"
        /// <summary>
        /// Gets the collection of Exif properties contained in the JPEG file.
        /// </summary>
        public Dictionary<ExifTag, ExifProperty> Properties { get; private set; }
        /// <summary>
        /// Gets or sets the byte-order of the Exif properties.
        /// </summary>
        public BitConverterEx.ByteOrder ByteOrder { get; set; }
        /// <summary>
        /// Gets or sets the embedded thumbnail image.
        /// </summary>
        public JPEGFile Thumbnail { get; set; }
        /// <summary>
        /// Gets or sets the Exif property with the given key.
        /// </summary>
        /// <param name="key">The Exif tag associated with the Exif property.</param>
        /// <returns></returns>
        public ExifProperty this[ExifTag key]
        {
            get
            {
                return Properties[key];
            }
            set
            {
                Properties[key] = value;
            }
        }
#if DEBUG__
        private MemoryBinStream mMap = new MemoryBinStream();
        [Browsable(false)]
        public MemoryBinStream Map { get { return mMap; } }
#endif
        #endregion

        #region "Constructors"
        protected ExifFile()
        {
            ;
        }
        #endregion

        #region "Instance Methods"
        /// <summary>
        /// Saves the JPEG/Exif image with the given filename.
        /// </summary>
        /// <param name="filename">The complete path to the JPEG/Exif file.</param>
        public void Save(string filename)
        {
            Save(filename, true);
        }

        /// <summary>
        /// Saves the JPEG/Exif image with the given filename.
        /// </summary>
        /// <param name="filename">The complete path to the JPEG/Exif file.</param>
        /// <param name="preserveMakerNote">Determines whether the maker note offset of
        /// the original file will be preserved.</param>
        public void Save(string filename, bool preserveMakerNote)
        {
            WriteApp1(preserveMakerNote);
            file.Save(filename);
        }

        /// <summary>
        /// Returns a System.Drawing.Bitmap created with image data.
        /// </summary>
        //public Bitmap ToBitmap()
        //{
        //    return file.ToBitmap();
        //}
        #endregion

        #region "Private Helper Methods"
        /// <summary>
        /// Reads the APP1 section containing Exif metadata.
        /// </summary>
        private void ReadAPP1()
        {
            // Find the APP1 section containing Exif metadata
            app1 = file.Sections.Find(a => (a.Marker == JPEGMarker.APP1) &&
                (Encoding.ASCII.GetString(a.Header, 0, 6) == "Exif\0\0"));

            // If there is no APP1 section, add a new one after the last APP0 section (if any).
            if (app1 == null)
            {
                int insertionIndex = file.Sections.FindLastIndex(a => a.Marker == JPEGMarker.APP0);
                if (insertionIndex == -1) insertionIndex = 0;
                insertionIndex++;
                ByteOrder = BitConverterEx.ByteOrder.BigEndian;
                app1 = new JPEGSection(JPEGMarker.APP1);
                file.Sections.Insert(insertionIndex, app1);
                return;
            }
#if DEBUG__
            mMap = new MemoryBinStream();
            mMap.Seek(0, SeekOrigin.Begin);
            mMap.Write(new Bin("Exif Marker", 3, 6));
#endif

            byte[] header = app1.Header;
            SortedList<int, IFD> ifdqueue = new SortedList<int, IFD>();
            makerNoteOffset = 0;

            // TIFF header
            int tiffoffset = 6;
            if (header[tiffoffset] == 0x49)
                ByteOrder = BitConverterEx.ByteOrder.LittleEndian;
            else
                ByteOrder = BitConverterEx.ByteOrder.BigEndian;
            BitConverterEx conv = new BitConverterEx(ByteOrder, BitConverterEx.ByteOrder.System);

            // Offset to 0th IFD
            int ifd0offset = (int)conv.ToUInt32(header, tiffoffset + 4);
            ifdqueue.Add(ifd0offset, IFD.Zeroth);

            int thumboffset = -1;
            int thumblength = 0;
            int thumbtype = -1;
#if DEBUG__
            mMap.Write(new Bin("Byte Order: " + ByteOrder.ToString(), 4, 2));
            mMap.Write(new Bin("TIFF ID: 0x00, 0x2A", 4, 2));
            mMap.Write(new Bin("Offset to 0th IFD: " + ifd0offset.ToString(), 4, 4));
#endif
            // Read IFDs
            while (ifdqueue.Count != 0)
            {
                int ifdoffset = tiffoffset + ifdqueue.Keys[0];
                IFD currentifd = ifdqueue.Values[0];
                ifdqueue.RemoveAt(0);

                // Field count
                ushort fieldcount = conv.ToUInt16(header, ifdoffset);
#if DEBUG__
                mMap.Seek(ifdoffset, SeekOrigin.Begin);
                mMap.Write(new Bin(currentifd.ToString() + " IFD Field Count: " + fieldcount.ToString(), 5, 2));
#endif
                for (short i = 0; i < fieldcount; i++)
                {
                    // Read field info
                    int fieldoffset = ifdoffset + 2 + 12 * i;
                    ushort tag = conv.ToUInt16(header, fieldoffset);
                    ushort type = conv.ToUInt16(header, fieldoffset + 2);
                    uint count = conv.ToUInt32(header, fieldoffset + 4);
                    byte[] value = new byte[4];
                    Array.Copy(header, fieldoffset + 8, value, 0, 4);

                    // Fields containing offsets to other IFDs
                    if (currentifd == IFD.Zeroth && tag == 0x8769)
                    {
                        int exififdpointer = (int)conv.ToUInt32(value, 0);
                        ifdqueue.Add(exififdpointer, IFD.EXIF);
                    }
                    else if (currentifd == IFD.Zeroth && tag == 0x8825)
                    {
                        int gpsifdpointer = (int)conv.ToUInt32(value, 0);
                        ifdqueue.Add(gpsifdpointer, IFD.GPS);
                    }
                    else if (currentifd == IFD.EXIF && tag == 0xa005)
                    {
                        int interopifdpointer = (int)conv.ToUInt32(value, 0);
                        ifdqueue.Add(interopifdpointer, IFD.Interop);
                    }

                    // Save the offset to maker note data
                    if (currentifd == IFD.EXIF && tag == 37500)
                        makerNoteOffset = conv.ToUInt32(value, 0);

                    // Calculate the bytes we need to read
                    uint baselength = 0;
                    if (type == 1 || type == 2 || type == 7)
                        baselength = 1;
                    else if (type == 3)
                        baselength = 2;
                    else if (type == 4 || type == 9)
                        baselength = 4;
                    else if (type == 5 || type == 10)
                        baselength = 8;
                    uint totallength = count * baselength;

                    // If field value does not fit in 4 bytes
                    // the value field is an offset to the actual
                    // field value
                    int fieldposition = 0;
                    if (totallength > 4)
                    {
                        fieldposition = tiffoffset + (int)conv.ToUInt32(value, 0);
                        value = new byte[totallength];
                        Array.Copy(header, fieldposition, value, 0, totallength);
                    }

                    // Compressed thumbnail data
                    if (currentifd == IFD.First && tag == 0x201)
                    {
                        thumbtype = 0;
                        thumboffset = (int)conv.ToUInt32(value, 0);
                    }
                    else if (currentifd == IFD.First && tag == 0x202)
                        thumblength = (int)conv.ToUInt32(value, 0);

                    // Uncompressed thumbnail data
                    if (currentifd == IFD.First && tag == 0x111)
                    {
                        thumbtype = 1;
                        // Offset to first strip
                        if (type == 3)
                            thumboffset = (int)conv.ToUInt16(value, 0);
                        else
                            thumboffset = (int)conv.ToUInt32(value, 0);
                    }
                    else if (currentifd == IFD.First && tag == 0x117)
                    {
                        thumblength = 0;
                        for (int j = 0; j < count; j++)
                        {
                            if (type == 3)
                                thumblength += (int)conv.ToUInt16(value, 0);
                            else
                                thumblength += (int)conv.ToUInt32(value, 0);
                        }
                    }

                    // Create the exif property from the interop data
                    ExifProperty prop = ExifPropertyFactory.Get(tag, type, count, value, ByteOrder, currentifd);
                    Properties.Add(prop.Tag, prop);
#if DEBUG__
                    mMap.Seek(fieldoffset, SeekOrigin.Begin);
                    mMap.Write(new Bin(ExifTagFactory.GetTagName(currentifd, tag) + " ID: " + tag.ToString(), 6, 2, prop));
                    mMap.Write(new Bin(ExifTagFactory.GetTagName(currentifd, tag) + " Type: " + type.ToString(), 6, 2, prop));
                    mMap.Write(new Bin(ExifTagFactory.GetTagName(currentifd, tag) + " Count: " + count.ToString(), 6, 4, prop));
                    mMap.Write(new Bin(ExifTagFactory.GetTagName(currentifd, tag) + " Value: " + string.Format("[0x{0:x2}, 0x{1:x2}, 0x{2:x2}, 0x{3:x2}]", value[0], value[1], value[2], value[3]), 6, 4, prop));
                    if (totallength > 4)
                    {
                        mMap.Seek(fieldposition, SeekOrigin.Begin);
                        mMap.Write(new Bin(ExifTagFactory.GetTagName(currentifd, tag) + " Data", 7, totallength, prop));
                    }
#endif
                }

                // 1st IFD pointer
                int firstifdpointer = (int)conv.ToUInt32(header, ifdoffset + 2 + 12 * fieldcount);
                if (firstifdpointer != 0)
                    ifdqueue.Add(firstifdpointer, IFD.First);
#if DEBUG__
                mMap.Seek(ifdoffset + 2 + 12 * fieldcount, SeekOrigin.Begin);
                mMap.Write(new Bin("1st IFD Pointer: " + firstifdpointer.ToString(), 5, 4));
#endif
                // Read thumbnail
                if (thumboffset != -1 && thumblength != 0 && Thumbnail == null)
                {
                    if (thumbtype == 0)
                    {
                        using (MemoryStream ts = new MemoryStream(header, tiffoffset + thumboffset, thumblength))
                        {
                            Thumbnail = new JPEGFile(ts);
                        }
                    }
#if DEBUG__
                    mMap.Seek(tiffoffset + thumboffset, SeekOrigin.Begin);
                    mMap.Write(new Bin("Thumbnail", 8, thumblength));
#endif
                }
            }
        }

        /// <summary>
        /// Replaces the contents of the APP1 section with the Exif properties.
        /// </summary>
        private void WriteApp1(bool preserveMakerNote)
        {
            // Zero out IFD field offsets. We will fill those as we write the IFD sections
            exifIFDFieldOffset = 0;
            gpsIFDFieldOffset = 0;
            interopIFDFieldOffset = 0;
            firstIFDFieldOffset = 0;
            // We also do not know the location of the embedded thumbnail yet
            thumbOffsetLocation = 0;
            thumbOffsetValue = 0;
            thumbSizeLocation = 0;
            thumbSizeValue = 0;

            // Which IFD sections do we have?
            Dictionary<ExifTag, ExifProperty> ifdzeroth = new Dictionary<ExifTag, ExifProperty>();
            Dictionary<ExifTag, ExifProperty> ifdexif = new Dictionary<ExifTag, ExifProperty>();
            Dictionary<ExifTag, ExifProperty> ifdgps = new Dictionary<ExifTag, ExifProperty>();
            Dictionary<ExifTag, ExifProperty> ifdinterop = new Dictionary<ExifTag, ExifProperty>();
            Dictionary<ExifTag, ExifProperty> ifdfirst = new Dictionary<ExifTag, ExifProperty>();

            foreach (KeyValuePair<ExifTag, ExifProperty> pair in Properties)
            {
                switch (pair.Value.IFD)
                {
                    case IFD.Zeroth:
                        ifdzeroth.Add(pair.Key, pair.Value);
                        break;
                    case IFD.EXIF:
                        ifdexif.Add(pair.Key, pair.Value);
                        break;
                    case IFD.GPS:
                        ifdgps.Add(pair.Key, pair.Value);
                        break;
                    case IFD.Interop:
                        ifdinterop.Add(pair.Key, pair.Value);
                        break;
                    case IFD.First:
                        ifdfirst.Add(pair.Key, pair.Value);
                        break;
                }
            }

            // Add IFD pointers if they are missing
            // We will write the pointer values later on
            if (ifdexif.Count != 0 && !ifdzeroth.ContainsKey(ExifTag.EXIFIFDPointer))
                ifdzeroth.Add(ExifTag.EXIFIFDPointer, new ExifUInt(ExifTag.EXIFIFDPointer, 0));
            if (ifdgps.Count != 0 && !ifdzeroth.ContainsKey(ExifTag.GPSIFDPointer))
                ifdzeroth.Add(ExifTag.GPSIFDPointer, new ExifUInt(ExifTag.GPSIFDPointer, 0));
            if (ifdinterop.Count != 0 && !ifdexif.ContainsKey(ExifTag.InteroperabilityIFDPointer))
                ifdexif.Add(ExifTag.InteroperabilityIFDPointer, new ExifUInt(ExifTag.InteroperabilityIFDPointer, 0));

            // Remove IFD pointers if IFD sections are missing
            if (ifdexif.Count == 0 && ifdzeroth.ContainsKey(ExifTag.EXIFIFDPointer))
                ifdzeroth.Remove(ExifTag.EXIFIFDPointer);
            if (ifdgps.Count == 0 && ifdzeroth.ContainsKey(ExifTag.GPSIFDPointer))
                ifdzeroth.Remove(ExifTag.GPSIFDPointer);
            if (ifdinterop.Count == 0 && ifdexif.ContainsKey(ExifTag.InteroperabilityIFDPointer))
                ifdexif.Remove(ExifTag.InteroperabilityIFDPointer);

            if (ifdzeroth.Count == 0)
                throw new IFD0IsEmptyException();

            // We will need these bitconverters to write byte-ordered data
            BitConverterEx bceExif = new BitConverterEx(BitConverterEx.ByteOrder.System, ByteOrder);

            // Create a memory stream to write the APP1 section to
            MemoryStream ms = new MemoryStream();

            // Exif identifer
            ms.Write(Encoding.ASCII.GetBytes("Exif\0\0"), 0, 6);

            // TIFF header
            // Byte order
            long tiffoffset = ms.Position;
            ms.Write((ByteOrder == BitConverterEx.ByteOrder.LittleEndian ? new byte[] { 0x49, 0x49 } : new byte[] { 0x4D, 0x4D }), 0, 2);
            // TIFF ID
            ms.Write(bceExif.GetBytes((ushort)42), 0, 2);
            // Offset to 0th IFD
            ms.Write(bceExif.GetBytes((uint)8), 0, 4);

            // Write IFDs
            WriteIFD(ms, ifdzeroth, IFD.Zeroth, tiffoffset, preserveMakerNote);
            uint exififdrelativeoffset = (uint)(ms.Position - tiffoffset);
            WriteIFD(ms, ifdexif, IFD.EXIF, tiffoffset, preserveMakerNote);
            uint gpsifdrelativeoffset = (uint)(ms.Position - tiffoffset);
            WriteIFD(ms, ifdgps, IFD.GPS, tiffoffset, preserveMakerNote);
            uint interopifdrelativeoffset = (uint)(ms.Position - tiffoffset);
            WriteIFD(ms, ifdinterop, IFD.Interop, tiffoffset, preserveMakerNote);
            uint firstifdrelativeoffset = (uint)(ms.Position - tiffoffset);
            WriteIFD(ms, ifdfirst, IFD.First, tiffoffset, preserveMakerNote);

            // Now that we now the location of IFDs we can go back and write IFD offsets
            if (exifIFDFieldOffset != 0)
            {
                ms.Seek(exifIFDFieldOffset, SeekOrigin.Begin);
                ms.Write(bceExif.GetBytes(exififdrelativeoffset), 0, 4);
            }
            if (gpsIFDFieldOffset != 0)
            {
                ms.Seek(gpsIFDFieldOffset, SeekOrigin.Begin);
                ms.Write(bceExif.GetBytes(gpsifdrelativeoffset), 0, 4);
            }
            if (interopIFDFieldOffset != 0)
            {
                ms.Seek(interopIFDFieldOffset, SeekOrigin.Begin);
                ms.Write(bceExif.GetBytes(interopifdrelativeoffset), 0, 4);
            }
            if (firstIFDFieldOffset != 0)
            {
                ms.Seek(firstIFDFieldOffset, SeekOrigin.Begin);
                ms.Write(bceExif.GetBytes(firstifdrelativeoffset), 0, 4);
            }
            // We can write thumbnail location now
            if (thumbOffsetLocation != 0)
            {
                ms.Seek(thumbOffsetLocation, SeekOrigin.Begin);
                ms.Write(bceExif.GetBytes(thumbOffsetValue), 0, 4);
            }
            if (thumbSizeLocation != 0)
            {
                ms.Seek(thumbSizeLocation, SeekOrigin.Begin);
                ms.Write(bceExif.GetBytes(thumbSizeValue), 0, 4);
            }

            ms.Close();

            // Return APP1 header
            app1.Header = ms.ToArray();
        }

        private void WriteIFD(MemoryStream stream, Dictionary<ExifTag, ExifProperty> ifd, IFD ifdtype, long tiffoffset, bool preserveMakerNote)
        {
            BitConverterEx conv = new BitConverterEx(BitConverterEx.ByteOrder.System, ByteOrder);

            // Create a queue of fields to write
            Queue<ExifProperty> fieldqueue = new Queue<ExifProperty>();
            foreach (ExifProperty prop in ifd.Values)
                if (prop.Tag != ExifTag.MakerNote)
                    fieldqueue.Enqueue(prop);
            // Push the maker note data to the end
            if (ifd.ContainsKey(ExifTag.MakerNote))
                fieldqueue.Enqueue(ifd[ExifTag.MakerNote]);

            // Offset to start of field data from start of TIFF header
            uint dataoffset = (uint)(2 + ifd.Count * 12 + 4 + stream.Position - tiffoffset);
            uint currentdataoffset = dataoffset;
            long absolutedataoffset = stream.Position + (2 + ifd.Count * 12 + 4);

            bool makernotewritten = false;
            // Field count
            stream.Write(conv.GetBytes((ushort)ifd.Count), 0, 2);
            // Fields
            while (fieldqueue.Count != 0)
            {
                ExifProperty field = fieldqueue.Dequeue();
                ExifInterOperability interop = field.Interoperability;

                uint fillerbytecount = 0;

                // Try to preserve the makernote data offset
                if (!makernotewritten &&
                    !makerNoteProcessed &&
                    makerNoteOffset != 0 &&
                    ifdtype == IFD.EXIF &&
                    field.Tag != ExifTag.MakerNote &&
                    interop.Data.Length > 4 &&
                    currentdataoffset + interop.Data.Length > makerNoteOffset &&
                    ifd.ContainsKey(ExifTag.MakerNote))
                {
                    // Delay writing this field until we write makernote data
                    fieldqueue.Enqueue(field);
                    continue;
                }
                else if (field.Tag == ExifTag.MakerNote)
                {
                    makernotewritten = true;
                    // We may need to write filler bytes to preserve maker note offset
                    if (preserveMakerNote && !makerNoteProcessed)
                        fillerbytecount = makerNoteOffset - currentdataoffset;
                    else
                        fillerbytecount = 0;
                }

                // Tag
                stream.Write(conv.GetBytes(interop.TagID), 0, 2);
                // Type
                stream.Write(conv.GetBytes(interop.TypeID), 0, 2);
                // Count
                stream.Write(conv.GetBytes(interop.Count), 0, 4);
                // Field data
                byte[] data = interop.Data;
                if (ByteOrder != BitConverterEx.SystemByteOrder)
                {
                    if (interop.TypeID == 1 || interop.TypeID == 3 || interop.TypeID == 4 || interop.TypeID == 9)
                        Array.Reverse(data);
                    else if (interop.TypeID == 5 || interop.TypeID == 10)
                    {
                        Array.Reverse(data, 0, 4);
                        Array.Reverse(data, 4, 4);
                    }
                }

                // Fields containing offsets to other IFDs
                // Just store their offets, we will write the values later on when we know the lengths of IFDs
                if (ifdtype == IFD.Zeroth && interop.TagID == 0x8769)
                    exifIFDFieldOffset = stream.Position;
                else if (ifdtype == IFD.Zeroth && interop.TagID == 0x8825)
                    gpsIFDFieldOffset = stream.Position;
                else if (ifdtype == IFD.EXIF && interop.TagID == 0xa005)
                    interopIFDFieldOffset = stream.Position;
                else if (ifdtype == IFD.First && interop.TagID == 0x201)
                    thumbOffsetLocation = stream.Position;
                else if (ifdtype == IFD.First && interop.TagID == 0x202)
                    thumbSizeLocation = stream.Position;

                // Write 4 byte field value or field data
                if (data.Length <= 4)
                {
                    stream.Write(data, 0, data.Length);
                    for (int i = data.Length; i < 4; i++)
                        stream.WriteByte(0);
                }
                else
                {
                    // Pointer to data area relative to TIFF header
                    stream.Write(conv.GetBytes(currentdataoffset + fillerbytecount), 0, 4);
                    // Actual data
                    long currentoffset = stream.Position;
                    stream.Seek(absolutedataoffset, SeekOrigin.Begin);
                    // Write filler bytes
                    for (int i = 0; i < fillerbytecount; i++)
                        stream.WriteByte(0xFF);
                    stream.Write(data, 0, data.Length);
                    stream.Seek(currentoffset, SeekOrigin.Begin);
                    // Increment pointers
                    currentdataoffset += fillerbytecount + (uint)data.Length;
                    absolutedataoffset += fillerbytecount + data.Length;
                }
            }
            // Offset to 1st IFD
            // We will write zeros for now. This will be filled after we write all IFDs
            if (ifdtype == IFD.Zeroth)
                firstIFDFieldOffset = stream.Position;
            stream.Write(new byte[] { 0, 0, 0, 0 }, 0, 4);

            // Seek to end of IFD
            stream.Seek(absolutedataoffset, SeekOrigin.Begin);

            // Write thumbnail data
            if (ifdtype == IFD.First)
            {
                if (Thumbnail != null)
                {
                    MemoryStream ts = new MemoryStream();
                    Thumbnail.Save(ts);
                    ts.Close();
                    byte[] thumb = ts.ToArray();
                    thumbOffsetValue = (uint)(stream.Position - tiffoffset);
                    thumbSizeValue = (uint)thumb.Length;
                    stream.Write(thumb, 0, thumb.Length);
                    ts.Dispose();
                }
                else
                {
                    thumbOffsetValue = 0;
                    thumbSizeValue = 0;
                }
            }
        }
        #endregion

        #region "Static Helper Methods"
        /// <summary>
        /// Creates a new ExifFile from the given JPEG/Exif image file.
        /// </summary>
        /// <param name="filename">Path to the JPEJ/Exif image file.</param>
        /// <returns>An ExifFile class initialized from the specified JPEG/Exif image file.</returns>
        public static ExifFile Read(string filename)
        {
            ExifFile exif = new ExifFile();

            // Read the JPEG file and process the APP1 section
            exif.Properties = new Dictionary<ExifTag, ExifProperty>();
            exif.file = new JPEGFile(filename);
            exif.ReadAPP1();

            // Process the maker note
            exif.makerNoteProcessed = false;

            return exif;
        }
		
		public static ExifFile Read(Stream fileStream)
        {
            ExifFile exif = new ExifFile();

            // Read the JPEG file and process the APP1 section
            exif.Properties = new Dictionary<ExifTag, ExifProperty>();
            exif.file = new JPEGFile();
			if(!exif.file.Read(fileStream))
			{
				exif.file = null;
				exif = null;
				return null;
			}
            exif.ReadAPP1();

            // Process the maker note
            exif.makerNoteProcessed = false;

            return exif;
        }
        #endregion
    }
}
#endif