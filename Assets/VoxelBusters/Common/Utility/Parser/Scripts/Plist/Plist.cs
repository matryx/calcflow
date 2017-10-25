using UnityEngine;
using System.Collections;

// Add DISABLE_PLIST_PARSER to Scripting Define Symbols, if you want to disable plist parser
#if !DISABLE_PLIST_PARSER && !(UNITY_WINRT || UNITY_WEBPLAYER || UNITY_WEBGL)
using System.Collections.Generic;
using System.IO;
using System.Xml;
using System.Xml.Linq;

namespace VoxelBusters.Utility
{
	public class Plist : Dictionary<string, object>
	{
		#region Static Methods

		public static Plist LoadPlistAtPath (string _filePath)
		{
			// Check if file exists
			if (!File.Exists(_filePath))
			{
				Debug.LogError("[Plist] Load failed as file doesnt exist, Path=" + _filePath);
				return null;
			}
		
			// Load file contents
			string _plistFileText	= File.ReadAllText(_filePath);
			Plist _plist 			= new Plist();

			_plist.ParsePlistText(_plistFileText); 
			return _plist;
		}

		public static Plist Load (string _plistTextContents)
		{
			// Load file contents
			Plist _plist 	= new Plist();

			_plist.ParsePlistText(_plistTextContents); 
			return _plist;
		}

		#endregion

		#region Constructor

		private Plist ()
		{}

		#endregion

		#region Parsing Methods

		private void ParsePlistText (string _text)
		{
			// Load XML
			XmlDocument _xmlDocument	= new XmlDocument();
			_xmlDocument.LoadXml(_text);
			
			XmlNode _plistNode 		= _xmlDocument.LastChild;
			XmlNode	_dictNode		= _plistNode.FirstChild;
			IDictionary _parsedDict	= (this as IDictionary);
			
			// Start parsing
			if (_dictNode != null)
				ParseDictionaryNode(_dictNode, ref _parsedDict);
		}

		private object ParseValueNode (XmlNode _valNode)
		{
			switch (_valNode.Name)
			{
			case "true":
				return true;
			case "false":
				return false;
			case "integer":
				return int.Parse(_valNode.InnerText);
			case "real":
				return float.Parse(_valNode.InnerText);
			case "string":
				return _valNode.InnerText;
			case "dict":
				IDictionary _parsedDict	= new Dictionary<string, object>();

				// Parse the node
				ParseDictionaryNode(_valNode, ref _parsedDict);
				return _parsedDict;
			case "array":
				IList _parsedList		= new List<object>();

				// Parse the node
				ParseListNode(_valNode, ref _parsedList);
				return _parsedList;
			}

			return null;
		}

		private void ParseListNode (XmlNode _listNode, ref IList _parsedList)
		{
			foreach (XmlNode _elementNode in _listNode.ChildNodes)
				_parsedList.Add(ParseValueNode(_elementNode));
		}

		private void ParseDictionaryNode (XmlNode _dictNode, ref IDictionary _parsedDict)
		{
			int _childCount			= _dictNode.ChildNodes.Count;
			
			for (int _iter = 0; _iter < _childCount; _iter += 2)
			{
				XmlNode _keyNode	= _dictNode.ChildNodes.Item(_iter);
				XmlNode _valNode	= _dictNode.ChildNodes.Item(_iter + 1);
				
				// Add to dictionary
				_parsedDict[_keyNode.InnerText]	= ParseValueNode(_valNode);
			}
		}

		#endregion

		#region Modifying Methods

		public object GetKeyPathValue (string _keyPath)
		{
			if (string.IsNullOrEmpty(_keyPath))
				return this;
			
			// Get list of keys from key path
			object _kpValue		= this;
			string[] _keyList	= _keyPath.Split('/');
			int _keysCount		= _keyList.Length;
			
			try
			{
				for (int _kIter = 0; _kIter < _keysCount; _kIter++)
				{
					string _key					= _keyList[_kIter];
					IDictionary _kpDictValue	= _kpValue as IDictionary;

					if (_kpDictValue == null || !_kpDictValue.Contains(_key))
					    throw new KeyNotFoundException();
					
					// Update value mapped to this key
					_kpValue	= _kpDictValue[_key];
				}

				return _kpValue;
			}
			catch (KeyNotFoundException _exception)
			{
				Debug.LogWarning("[Plist] " + _exception.Message);
				return null;
			}
		}

		public void AddValue (string _keyPath, object _newValue)
		{
			if (_keyPath == null)
				_keyPath	= string.Empty;

			// Get list of keys from key path
			string[] _keyList		= _keyPath.Split('/');
			string _innermostKey	= _keyList[_keyList.Length -1];
			string _kpOuterKey		= _keyPath.Substring(0, _keyPath.Length - _innermostKey.Length).TrimEnd('/');
			IDictionary _kpValue	= GetKeyPathValue(_kpOuterKey) as IDictionary;

			// Adding new value
			if (_kpValue != null)
				_kpValue[_innermostKey] = _newValue;
		}

		#endregion

		#region Saving Methods

		public void Save (string _saveToPath)
		{
			using (StreamWriter _writer = new StreamWriter(_saveToPath))
			{
				using (MemoryStream _memStream = new MemoryStream())
				{
					XmlWriterSettings _xmlWriterSettings 	= new XmlWriterSettings();
					_xmlWriterSettings.Encoding 			= new System.Text.UTF8Encoding(true);
					_xmlWriterSettings.ConformanceLevel 	= ConformanceLevel.Document;
					_xmlWriterSettings.Indent 				= true;
					
					using (XmlWriter xmlWriter = XmlWriter.Create(_memStream, _xmlWriterSettings))
					{
						xmlWriter.WriteStartDocument(); 

						// Add this only if we are saving it in property list format
						xmlWriter.WriteDocType("plist", "-//Apple Computer//DTD PLIST 1.0//EN", "http://www.apple.com/DTDs/Plist-1.0.dtd", null);

						// We are using format v1.0
						xmlWriter.WriteStartElement("plist");
						xmlWriter.WriteAttributeString("version", "1.0");

						// Write xml contents
						WriteXMLDictionaryNode(xmlWriter, this);

						// End element
						xmlWriter.WriteEndElement();

						// End document
						xmlWriter.WriteEndDocument();
						xmlWriter.Flush();
						xmlWriter.Close();

						// Writing XML contents
						string _xmlContents	= System.Text.Encoding.UTF8.GetString(_memStream.ToArray());
						_writer.Write(_xmlContents);
					}
				}
			}
		}

		private void WriteXMLNode (XmlWriter _xmlWriter, object _value)
		{
			if (_value is bool)
			{
				WriteXMLBoolNode(_xmlWriter, (bool)_value);
			}
			else if (_value is int || _value is long)
			{
				WriteXMLIntegerNode(_xmlWriter, (int)_value);
			}
			else if ((_value as string) != null)
			{
				WriteXMLStringNode(_xmlWriter, _value as string);
			}
			else if ((_value as IList) != null)
			{
				WriteXMLListNode(_xmlWriter, _value as IList);
			}
			else if ((_value as IDictionary) != null)
			{
				WriteXMLDictionaryNode(_xmlWriter, _value as IDictionary);
			}
		}

		private void WriteXMLBoolNode (XmlWriter _xmlWriter, bool _value)
		{
			_xmlWriter.WriteElementString(_value.ToString().ToLower(), "");
		}
		
		private void WriteXMLIntegerNode (XmlWriter _xmlWriter, int _value)
		{
			_xmlWriter.WriteElementString("integer", _value.ToString(System.Globalization.NumberFormatInfo.InvariantInfo));
		}

		private void WriteXMLStringNode (XmlWriter _xmlWriter, string _value)
		{
			_xmlWriter.WriteElementString("string", _value);
		}
		
		private void WriteXMLListNode (XmlWriter _xmlWriter, IList _listValue)
		{
			if (_listValue == null)
				_listValue = new List<object>();

			// Array element starts
			_xmlWriter.WriteStartElement("array");
			
			foreach (object _element in _listValue)
				WriteXMLNode(_xmlWriter, _element);
			
			// Array element ends
			_xmlWriter.WriteEndElement();
		}

		private void WriteXMLDictionaryNode (XmlWriter _xmlWriter, IDictionary _dictValue)
		{
			if (_dictValue == null)
				_dictValue = new Dictionary<string, object>();

			// Dictionary element starts
			_xmlWriter.WriteStartElement("dict");

			foreach (object _key in _dictValue.Keys)
			{
				_xmlWriter.WriteElementString("key", _key.ToString());
				WriteXMLNode(_xmlWriter, _dictValue[_key]);
			}

			// Dictionary element ends
			_xmlWriter.WriteEndElement();
		}

		#endregion
	}
}
#endif