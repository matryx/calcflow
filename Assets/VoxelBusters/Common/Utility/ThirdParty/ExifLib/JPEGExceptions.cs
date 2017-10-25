using System;
using System.Collections.Generic;
using System.Text;

namespace ExifLibrary
{
    /// <summary>
    /// The exception that is thrown when the format of the JPEG file
    /// could not be understood.
    /// </summary>
    public class NotValidJPEGFileException : Exception
    {
        public NotValidJPEGFileException()
            : base("Not a valid JPEG file.")
        {
            ;
        }

        public NotValidJPEGFileException(string message)
            : base(message)
        {
            ;
        }
    }

    /// <summary>
    /// The exception that is thrown when the length of a section exceeds 64 kB.
    /// </summary>
    public class SectionExceeds64KBException : Exception
    {
        public SectionExceeds64KBException()
            : base("Section length exceeds 64 kB.")
        {
            ;
        }

        public SectionExceeds64KBException(string message)
            : base(message)
        {
            ;
        }
    }
}
