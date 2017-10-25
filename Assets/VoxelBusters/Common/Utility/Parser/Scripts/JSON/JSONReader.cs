using UnityEngine;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System;

namespace VoxelBusters.Utility
{
	using Internal;

	public class JSONReader 
	{
		#region Properties

		public 		JSONString			JSONString
		{
			get;
			private set;
		}

		#endregion

		#region Constructors

		private JSONReader ()
		{}

		public JSONReader (string _inputJSONString)
		{
			JSONString	= new JSONString(_inputJSONString);
		}

		#endregion

		#region Methods

		public object Deserialise ()
		{
			// Check if input string is null
			if (JSONString.IsNullOrEmpty)
				return null;

			int _index	= 0;

			return ReadValue(ref _index);
		}

		public object Deserialise (ref int _errorIndex)
		{
			// Check if input string is null
			if (JSONString.IsNullOrEmpty)
				return null;

			// Read JSON string 
			int _index			= 0;
			object _value		= ReadValue(ref _index);
			
			if (_index != JSONString.Length)
				_errorIndex	= _index;
			else
				_errorIndex	= -1;

			return _value;
		}

		public object ReadValue (ref int _index)
		{
			// Remove white spaces
			RemoveWhiteSpace(ref _index);

			// Look ahead
			eJSONToken	_token	= LookAhead(_index);

			switch (_token)
			{
			case eJSONToken.CURLY_OPEN: 
				return ReadObject(ref _index);
				
			case eJSONToken.SQUARED_OPEN:
				return ReadArray(ref _index);
				
			case eJSONToken.STRING:
				return ReadString(ref _index);
				
			case eJSONToken.NUMBER:
				return ReadNumber(ref _index);
				
			case eJSONToken.NULL:
				_index	+= 4;
				return null;
				
			case eJSONToken.TRUE:
				_index += 4;
				return true;
				
			case eJSONToken.FALSE:
				_index += 5;
				return false;
				
			default:
				Debug.LogError(string.Format("[JSON] Parse error at index ={0}", _index));
				break;
			}
			
			return null;
		}

		#endregion

		#region Parse Dictionary Methods
		
		public object ReadObject (ref int _index)
		{
			IDictionary 		_dictionary 	= new Dictionary<string, object>();
			bool 				_done 			= false;
			
			// Skip curls
			_index++;
			
			while (!_done) 
			{
				eJSONToken 		_token = LookAhead(_index);
				
				if (_token == eJSONToken.NONE) 
				{
					Debug.LogError(string.Format("[JSON] Parse error at index ={0}", _index));
					return null;
				} 
				else if (_token == eJSONToken.CURLY_CLOSE) 
				{
					NextToken(ref _index);
					
					// Mark read dictionary object as finished
					_done						= true;
				}
				else 
				{
					string 		_key;
					object 		_value;
					int			_readStatus		= ReadKeyValuePair(ref _index, out _key, out _value);

					if (_readStatus != -1)
					{
						// Add dictionary entry
						_dictionary[_key]		= _value;

						// Read next token
						eJSONToken	_nextToken	= LookAhead(_index);

						if (_nextToken == eJSONToken.COMMA) 
						{
							NextToken(ref _index);
						} 
						else if (_nextToken == eJSONToken.CURLY_CLOSE) 
						{
							NextToken(ref _index);

							// Mark read dictionary object as finished
							_done				= true;
						}
						else
						{
							Debug.LogError(string.Format("[JSON] Parse error at index ={0}", _index));
							return null;
						}
					}
				}
			}
			
			return _dictionary;
		}

		public int ReadKeyValuePair (ref int _index, out string _key, out object _value)
		{
			// Default values
			_key	= null;
			_value	= null;

			// Read key
			_key	= ReadValue(ref _index) as string;

			// Check if we have a valid key or not
			if (_key == null) 
			{
				Debug.LogError(string.Format("[JSON] Parse error at index ={0}", _index));
				return -1;
			}

			// Next token should be colon
			if (NextToken(ref _index) != eJSONToken.COLON) 
			{
				Debug.LogError(string.Format("[JSON] Parse error at index ={0}", _index));
				return -1;
			}

			// Read value
			_value	= ReadValue(ref _index);

			return 0;
		}

		#endregion

		#region Parse List Methods
		
		public object ReadArray (ref int _index)
		{
			IList			_arraylist 		= new List<object>();
			bool 			_done 			= false;
			
			// Skip square bracket
			_index++;
			
			while (!_done) 
			{
				eJSONToken 	_token 			= LookAhead(_index);
				
				if (_token == eJSONToken.NONE) 
				{
					Debug.LogError(string.Format("[JSON] Parse error at index ={0}", _index));
					return null;
				}
				else if (_token == eJSONToken.SQUARED_CLOSE) 
				{
					NextToken(ref _index);
					
					// Mark read array object as finished
					_done					= true;
				} 
				else 
				{
					// Read array element
					object		_arrayElement;

					ReadArrayElement(ref _index, out _arrayElement);
					_arraylist.Add(_arrayElement);

					// Read next token
					eJSONToken	_nextToken	= LookAhead(_index);

					if (_nextToken == eJSONToken.COMMA) 
					{
						NextToken(ref _index);
					} 
					else if (_nextToken == eJSONToken.SQUARED_CLOSE) 
					{
						NextToken(ref _index);

						// Mark read array object as finished
						_done				= true;
					} 
					else
					{
						Debug.LogError(string.Format("[JSON] Parse error at index ={0}", _index));
						return null;
					}
				}
			}
			
			return _arraylist;
		}

		public void ReadArrayElement (ref int _index, out object _element)
		{
			_element	= ReadValue(ref _index);
		}

		#endregion

		#region Parse String Numeric Method
		
		public string ReadString (ref int _index)
		{
			StringBuilder _strbuilder	= new StringBuilder();
			bool _done					= false;
			
			// Skip double quotes
			_index++;
			
			while (!_done)
			{
				// We are done with the json string
				if (_index == JSONString.Length)
					break;
				
				// Get current character and increment pointer index
				char _curChar	= JSONString[_index++];
				
				// We reached end of our string
				if (_curChar == '"')
				{
					_done	= true;
				}
				else if (_curChar == '\\')
				{
					// We are done with the json string
					if (_index == JSONString.Length)
						break;
					
					// Get current character and increment pointer index
					_curChar	= JSONString[_index++];
					
					if (_curChar == '"') 
						_strbuilder.Append('"');
					else if (_curChar == '\\') 
						_strbuilder.Append('\\');
					else if (_curChar == '/') 
						_strbuilder.Append('/');
					else if (_curChar == 'b') 
						_strbuilder.Append('\b');
					else if (_curChar == 'f') 
						_strbuilder.Append('\f');
					else if (_curChar == 'n')
						_strbuilder.Append('\n');
					else if (_curChar == 'r')
						_strbuilder.Append('\r');
					else if (_curChar == 't')
						_strbuilder.Append('\t');
					else if (_curChar == 'u') 
					{
						int _remLength = JSONString.Length - _index;
						
						if (_remLength >= 4) 
						{
							string _unicodeStr 	= JSONString.Value.Substring(_index, 4);
							
							// Append unicode char to string 
							char _unicodeChar	= (char)int.Parse(_unicodeStr, System.Globalization.NumberStyles.HexNumber);
							
							_strbuilder.Append(_unicodeChar);
							
							// Skip next 4 characters
							_index += 4;
						} 
						else 
						{
							break;
						}					
					}
				}
				else 
				{
					_strbuilder.Append(_curChar);
				}
			}
			
			if (!_done) 
				return null;
			
			return _strbuilder.ToString();
		}
		
		public object ReadNumber (ref int _index)
		{
			int _numIndex	= _index;
			bool _done		= false;
			
			while (!_done)
			{
				if (JSONConstants.kNumericLiterals.IndexOf(JSONString[_numIndex]) != -1)
				{
					_numIndex++;

					if (_numIndex >= JSONString.Length)
						_done	= true;
				}
				else
				{
					_done	= true;
				}
			}
			
			// Get number sequence
			int _strLength			= _numIndex - _index;
			string _numberString	= JSONString.Value.Substring(_index, _strLength);
			
			// Update look ahead index, point it to next char after end of number string
			_index					= _numIndex;
			
			// First try to parse an number as long int
			long _longValue;
			
			if (long.TryParse(_numberString, out _longValue))
				return _longValue;
			
			// As long int parsing failed, try parsing it as double
			double _doubleValue;
			
			if (double.TryParse(_numberString, out _doubleValue))
				return _doubleValue;
			
			return null;
		}

		#endregion

		#region Helper Methods
		
		public eJSONToken LookAhead (int _index)
		{
			int _indexCopy	= _index;
			
			// Find next token without affecting indexing
			return NextToken(ref _indexCopy);
		}
		
		public eJSONToken NextToken (ref int _index)
		{
			// Check if exceeded json string length
			if (_index == JSONString.Length) 
				return eJSONToken.NONE;
			
			// Remove spacing
			RemoveWhiteSpace(ref _index);
			
			// Cache current character
			char _char	= JSONString[_index++];
			
			switch (_char)
			{
			case '{': 
				return eJSONToken.CURLY_OPEN;
				
			case '}': 
				return eJSONToken.CURLY_CLOSE;
				
			case '[': 
				return eJSONToken.SQUARED_OPEN;
				
			case ']': 
				return eJSONToken.SQUARED_CLOSE;
				
			case ':': 
				return eJSONToken.COLON;
				
			case ',': 
				return eJSONToken.COMMA;
				
			case '"': 
				return eJSONToken.STRING;
				
			case '0': case '1': case '2': case '3': case '4': 
			case '5': case '6': case '7': case '8': case '9':
			case '-': 
				return eJSONToken.NUMBER;
			}
			
			// Reverting post increment which was done after reading character
			_index--;
			
			// Can be null data
			if ((_index + 4) < JSONString.Length)
			{
				if (('n' == JSONString[_index]) &&
				    ('u' == JSONString[_index + 1]) &&
				    ('l' == JSONString[_index + 2]) &&
				    ('l' == JSONString[_index + 3]))
				{
					_index += 4;
					return eJSONToken.NULL;
				}
			}
			
			// Can be boolean true
			if ((_index + 4) < JSONString.Length)
			{
				if (('t' == JSONString[_index]) &&
				    ('r' == JSONString[_index + 1]) &&
				    ('u' == JSONString[_index + 2]) &&
				    ('e' == JSONString[_index + 3]))
				{
					_index += 4;
					return eJSONToken.TRUE;
				}
			}
			
			// Can be boolean false
			if ((_index + 5) < JSONString.Length)
			{
				if (('f' == JSONString[_index]) &&
				    ('a' == JSONString[_index + 1]) &&
				    ('l' == JSONString[_index + 2]) &&
				    ('s' == JSONString[_index + 3]) &&
				    ('e' == JSONString[_index + 4]))
				{
					_index += 5;
					return eJSONToken.FALSE;
				}
			}
			
			return eJSONToken.NONE;
		}
		
		private void RemoveWhiteSpace (ref int _index)
		{
			int _charCount	= JSONString.Length;
			
			while (_index < _charCount)
			{
				char _char	= JSONString[_index];
				
				if (JSONConstants.kWhiteSpaceLiterals.IndexOf(_char) != -1)
				{
					_index++;
				}
				else
				{
					break;
				}
			}
		}

		#endregion
	}
}
