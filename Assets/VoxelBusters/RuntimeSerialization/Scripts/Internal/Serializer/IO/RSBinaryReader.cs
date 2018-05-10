using UnityEngine;
using System.Collections;
using System;
using System.IO;
using System.Globalization;

namespace VoxelBusters.RuntimeSerialization.Internal
{
	internal class RSBinaryReader : BinaryReader
	{

		#region Constructors
		
		internal RSBinaryReader (int _bufferCapacity) : base (new MemoryStream(_bufferCapacity))
		{}

		internal RSBinaryReader (Stream _stream) : base (_stream)
		{}
		
		#endregion

		#region Methods
		
		internal void Reset ()
		{
			MemoryStream	_stream 	= this.BaseStream as MemoryStream;

			if (_stream == null)
				throw new Exception("[RS] Memory stream cant be null.");

			// Sets Position to start position and sets Length to 0
			_stream.SetLength(0);
		}
		
		internal void LoadBuffer (byte[] _buffer)
		{
			if (_buffer == null)
				throw new Exception("[RS] Buffer cant be null.");
			
			MemoryStream	_stream 	= this.BaseStream as MemoryStream;

			if (_stream == null)
				throw new Exception("[RS] Memory stream cant be null.");

			long 			_oldPos		= _stream.Position;

			// Load all the byte data
			_stream.Write(_buffer, 0, _buffer.Length);

			// Set position of stream to beginning
			_stream.Seek(_oldPos, SeekOrigin.Begin);
		}

		#endregion

		#region Read Methods

		internal object ReadPrimitiveValue (TypeCode _typeCode)
		{
			switch (_typeCode)
			{
			case TypeCode.Boolean:
				return ReadBoolean();

			case TypeCode.Char:
				return ReadChar();
			
			case TypeCode.SByte:
				return ReadSByte();
			
			case TypeCode.Byte:
				return ReadByte();
			
			case TypeCode.Int16:
				return ReadInt16();
			
			case TypeCode.UInt16:
				return ReadUInt16();
			
			case TypeCode.Int32:
				return ReadInt32();
			
			case TypeCode.UInt32:
				return ReadUInt32();
			
			case TypeCode.Int64:
				return ReadInt64();
			
			case TypeCode.UInt64:
				return ReadUInt64();
			
			case TypeCode.Single:
				return ReadSingle();
			
			case TypeCode.Double:
				return ReadDouble();
			
			case TypeCode.Decimal:
				return decimal.Parse(ReadString(), CultureInfo.InvariantCulture);
			
			case TypeCode.DateTime:
				return DateTime.FromBinary(ReadInt64());
			
			case TypeCode.String:
				return ReadString();

			default:
				throw new NotSupportedException ("[RS] Unsupported primitive type code=" + _typeCode);
			}
		}

		internal object ReadEnumValue (Type _enumType)
		{
			TypeCode _typeCode	= (TypeCode)ReadInt32();

			return Enum.ToObject(_enumType, ReadPrimitiveValue(_typeCode));
		}

		internal BinaryElement ReadBinaryElement ()
		{
			return (BinaryElement)ReadByte();
		}
		
		internal eTypeTag ReadTypeTag ()
		{
			return (eTypeTag)ReadByte();
		}

		#endregion
	}
}