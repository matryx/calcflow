using UnityEngine;
using System.Collections;
using System;
using System.IO;
using System.Globalization;

namespace VoxelBusters.RuntimeSerialization.Internal
{
	internal class RSBinaryWriter : BinaryWriter
	{
		#region Constructors

		internal RSBinaryWriter (int _bufferCapacity) : base (new MemoryStream(_bufferCapacity))
		{}

		internal RSBinaryWriter (Stream _stream) : base (_stream)
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

		internal string ToBase64String ()
		{
			MemoryStream	_stream 	= this.BaseStream as MemoryStream;
			
			if (_stream == null)
				throw new Exception("[RS] Memory stream cant be null.");

			return Convert.ToBase64String(_stream.GetBuffer(), 0, (int)_stream.Length);
		}

		internal byte[] ToBytes ()
		{
			MemoryStream	_stream 	= this.BaseStream as MemoryStream;
			
			if (_stream == null)
				throw new Exception("[RS] Memory stream cant be null.");
			
			return _stream.ToArray();
		}

		#endregion

		#region Write Methods

		internal void WritePrimitiveValue (object _value, TypeCode _typeCode)
		{
			switch (_typeCode)
			{
			case TypeCode.Boolean:
				Write((bool)_value);
				return;

			case TypeCode.Char:
				Write((char)_value);
				return;

			case TypeCode.SByte:
				Write((sbyte)_value);
				return;

			case TypeCode.Byte:
				Write((byte)_value);
				return;

			case TypeCode.Int16:
				Write((short)_value);
				return;

			case TypeCode.UInt16:
				Write((ushort)_value);
				return;

			case TypeCode.Int32:
				Write((int)_value);
				return;

			case TypeCode.UInt32:
				Write((uint)_value);
				return;

			case TypeCode.Int64:
				Write((long)_value);
				return;

			case TypeCode.UInt64:
				Write((ulong)_value);
				return;

			case TypeCode.Single:
				Write((float)_value);
				return;

			case TypeCode.Double:
				Write((double)_value);
				return;

			case TypeCode.Decimal:
				Write(((decimal)_value).ToString(CultureInfo.InvariantCulture));
				return;

			case TypeCode.DateTime:
				Write(((DateTime)_value).ToBinary());
				return;

			case TypeCode.String:
				Write(_value as string);
				return;

			default:
				throw new NotSupportedException ("[RS] Unsupported primitive type code=" + _typeCode);
			}
		}

		internal void WriteEnumValue (object _value, Type _type)
		{
			TypeCode _typecode	= Type.GetTypeCode(_type);

			// Write enum value
			Write((int)_typecode);
			WritePrimitiveValue(_value, _typecode);
		}

		internal void WriteBinaryElement (BinaryElement _element)
		{
			Write((byte)_element);
		}

		internal void WriteTypeTag (eTypeTag _tag)
		{
			Write((byte)_tag);
		}

		#endregion
	}
}