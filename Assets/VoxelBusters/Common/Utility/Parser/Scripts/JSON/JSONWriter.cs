using UnityEngine;
using System.Collections;
using System;
using System.Text;

#if NETFX_CORE
using System.Reflection;
#endif

namespace VoxelBusters.Utility
{
	using Internal;

	public class JSONWriter 
	{
		#region Constants

		private const		int 					kBufferLength	= 512;

		#endregion

		#region Properties

		internal 			StringBuilder			StringBuilder
		{
			get;
			private set;
		}

		#endregion

		#region Constructors

		public JSONWriter (int _bufferLength = kBufferLength)
		{
			StringBuilder		= new StringBuilder(_bufferLength);
		}

		#endregion

		#region Methods

		public string Serialise (object _objectValue)
		{
			WriteObjectValue(_objectValue);

			return StringBuilder.ToString();
		}

		public void WriteObjectValue (object _objectVal)
		{	
			// Null value
			if (_objectVal	== null)
			{
				WriteNullValue();
				return;
			}
			
			Type _objectType	= _objectVal.GetType();

#if !NETFX_CORE
			if (_objectType.IsPrimitive)
#else
			if (_objectType.GetTypeInfo().IsPrimitive)
#endif
			{
				WritePrimitive(_objectVal);
				return;
			}
			// Enum type
#if !NETFX_CORE
			else if (_objectType.IsEnum)
#else
			else if (_objectType.GetTypeInfo().IsEnum)
#endif
			{
				WriteEnum(_objectVal);
				return;
			}
			// Array type
			else if (_objectType.IsArray)
			{
				WriteArray(_objectVal as System.Array);
				return;
			}
			// Generic list type
			else if (_objectVal as IList != null)
			{
				WriteList(_objectVal as IList);
				return;
			}
			// Generic dictionary type
			else if (_objectVal as IDictionary != null)
			{
				WriteDictionary(_objectVal as IDictionary);
				return;
			}
			
			// Other types
			WriteString(_objectVal.ToString());
			return;
		}
		
		public void WriteDictionary (IDictionary _dict)
		{
			bool _firstEntry						= true;
			IDictionaryEnumerator _dictEnumerator	= _dict.GetEnumerator();
			
			// Initialise with symbol to indicate start of hash
			StringBuilder.Append('{');
			
			// Iterate through all keys
			while (_dictEnumerator.MoveNext())
			{
				// Append element seperator
				if (_firstEntry)
					_firstEntry	= false;
				else
					StringBuilder.Append(',');

				// Key value pair is shown as key:value
				WriteString(_dictEnumerator.Key.ToString());	
				StringBuilder.Append(':');				
				WriteObjectValue(_dictEnumerator.Value);
			}
			
			// Append symbol to indicate end of json string representation of dictionary
			StringBuilder.Append('}');
			return;
		}
		
		public void WriteArray (System.Array _array)
		{
			// Initialise with symbol to indicate start of array
			StringBuilder.Append('[');

			switch (_array.Rank)
			{
			case 1:
				int _1DArrayLength				= _array.Length;

				for (int _iter = 0; _iter < _1DArrayLength; _iter++) 
				{
					if (_iter != 0)
						StringBuilder.Append(',');

					WriteObjectValue(_array.GetValue(_iter));
				}
				
				break;
				
			case 2:
				int _outerArrayLength				= _array.GetLength(0);
				int _innerArrayLength				= _array.GetLength(1);
				
				for (int _outerIter = 0; _outerIter < _outerArrayLength; _outerIter++)
				{
					if (_outerIter != 0)
						StringBuilder.Append(',');

					// Append symbol to indicate start of json string representation of inner array
					StringBuilder.Append('[');

					for (int _innerIter = 0; _innerIter < _innerArrayLength; _innerIter++)
					{
						if (_innerIter != 0)
							StringBuilder.Append(',');

						WriteObjectValue(_array.GetValue(_outerIter, _innerIter));
					}
					
					// Append symbol to indicate end of json string representation of inner array
					StringBuilder.Append(']');
				}
				
				break;
			}
			
			// Append symbol to indicate end of json string representation of array
			StringBuilder.Append(']');
			return;
		}
		
		public void WriteList (IList _list)
		{
			int _totalCount		= _list.Count;

			// Initialise with symbol to indicate start of list
			StringBuilder.Append('[');

			for (int _iter = 0; _iter < _totalCount; _iter++) 
			{
				// Append element seperator
				if (_iter != 0)
					StringBuilder.Append(',');

				WriteObjectValue(_list[_iter]);
			}
			
			// Append symbol to indicate end of json string representation of array
			StringBuilder.Append(']');
			return;
		}

		public void WritePrimitive (object _value)
		{
			if (_value is bool)
			{
				if ((bool)_value)
					StringBuilder.Append(JSONConstants.kBoolTrue);
				else
					StringBuilder.Append(JSONConstants.kBoolFalse);
			}
			else if (_value is char)
			{
				StringBuilder.Append('"').Append((char)_value).Append('"');
			}
			else
			{
				StringBuilder.Append(_value);
			}
		}
		
		public void WriteEnum (object _value)
		{
			StringBuilder.Append((int)_value);
		}

		public void WriteNullValue ()
		{
			StringBuilder.Append(JSONConstants.kNull);
		}
		
		public void WriteString (string _stringVal)
		{
			int _charCount	= _stringVal.Length;
			int _charIter	= 0;
			
			// Append quotes to indicate start of string
			StringBuilder.Append('"');
			
			while (_charIter < _charCount)
			{
				char _char	= _stringVal[_charIter++];
				
				if (_char == '"') 
				{
					StringBuilder.Append('\\').Append('"');
				}
				else if (_char == '\\') 
				{
					StringBuilder.Append('\\').Append('\\');
				}
				else if (_char == '/') 
				{
					StringBuilder.Append('\\').Append('/');
				}
				else if (_char == '\b') 
				{
					StringBuilder.Append('\\').Append('b');
				}
				else if (_char == '\f') 
				{
					StringBuilder.Append('\\').Append('f');
				}
				else if (_char == '\n')
				{
					StringBuilder.Append('\\').Append('n');
				}
				else if (_char == '\r')
				{
					StringBuilder.Append('\\').Append('r');
				}
				else if (_char == '\t')
				{
					StringBuilder.Append('\\').Append('t');
				}
				else if (_char > 127) 
				{	
					string _unicode = "\\u" + ((int)_char).ToString("x4");
					StringBuilder.Append(_unicode);
				}
				else
				{
					StringBuilder.Append(_char);
				}
			}
			
			// Append quotes to indicate end of string
			StringBuilder.Append('"');
		}
		
		#endregion

		#region Dictionary Methods
		
		public void WriteDictionaryStart ()
		{
			StringBuilder.Append('{');
		}

		public void WriteKeyValuePair (string _key, object _value, bool _appendSeperator = false)
		{
			// Key value pair is shown in "key":"value" format
			WriteString(_key);
			StringBuilder.Append(':');
			WriteObjectValue(_value);

			// Append seperator
			if (_appendSeperator)
				StringBuilder.Append(',');
		}

		public void WriteKeyValuePairSeperator ()
		{
			StringBuilder.Append(':');
		}
		
		public void WriteDictionaryEnd ()
		{
			StringBuilder.Append('}');
		}

		#endregion

		#region Array / List Methods
		
		public void WriteArrayStart ()
		{
			StringBuilder.Append('[');
		}
		
		public void WriteArrayEnd ()
		{
			StringBuilder.Append(']');
		}

		#endregion

		#region Misc Methods

		public void WriteElementSeperator ()
		{
			StringBuilder.Append(',');
		}

		#endregion
	}
}