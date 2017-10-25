using UnityEngine;
using System.Collections;

#if UNITY_EDITOR && !(UNITY_WEBPLAYER || UNITY_WEBGL)
using UnityEditor;
using System;
using System.Collections.Generic;
using System.Xml;
using System.IO;
using System.Reflection;
using System.Linq;

namespace VoxelBusters.Utility
{
	public partial class StrippingBlacklistGenerator : AssetPostprocessor
	{
		#region Properties

		private				List<BlacklistedAssembly>			m_blacklistedAssemblyList;

		#endregion

		#region Constants

		private				string								kSavePath						= "Assets/link.xml";

		#endregion

		#region Constructors

		public StrippingBlacklistGenerator ()
		{
			// Initialize
			m_blacklistedAssemblyList	= new List<BlacklistedAssembly>();
		}

		#endregion

		#region Static Method

#if !DISABLE_STRIPPING_BLACKLIST_GENERATOR
		private static void OnPostprocessAllAssets (string[] importedAssets, string[] deletedAssets, string[] movedAssets, string[] movedFromAssetPaths) 
		{
			StrippingBlacklistGenerator _generator	= new StrippingBlacklistGenerator();

			// Load all link files and merge to one file
			_generator.FindAndMergeLinkFiles();
			_generator.SaveLinkerFile();
		}
#endif

		#endregion

		#region Methods

		private void LoadLinkerFileData (string _path)
		{
			// Parse existing link xml file
			if (!FileOperations.Exists(_path))
				return;

			using (XmlReader _xmlReader	= XmlReader.Create(_path))
			{
				while (_xmlReader.Read())
				{
					switch (_xmlReader.NodeType) 
					{
					case XmlNodeType.Element:
						if (_xmlReader.Name.Equals("assembly"))
							ParseAssemblyNode(_xmlReader);
						break;

					default:
						break;
					}
				}
			}
		}

		private void ParseAssemblyNode (XmlReader _xmlReader)
		{
			if (!_xmlReader.HasAttributes)
				return;

			string					_assemblyFullName	= _xmlReader.GetAttribute("fullname");
				
			// Add to the black list info dictionary
			BlacklistedAssembly		_assembly			= GetBlacklistedAssembly(_assemblyFullName);

			while (_xmlReader.Read())
			{
				switch (_xmlReader.NodeType) 
				{
				case XmlNodeType.Element:
					if (_xmlReader.Name.Equals("type"))
						_assembly.BlackListType(_xmlReader.GetAttribute("fullname"));
					else if (_xmlReader.Name.Equals("namespace"))
						_assembly.BlackListNamespace(_xmlReader.GetAttribute("fullname"));

					break;

				case XmlNodeType.EndElement:
					if (_xmlReader.Name.Equals("assembly"))
						return;
					break;

				default:
					break;
				}
			}
		}

		public void FindAndMergeLinkFiles ()
		{
			string			_dataPath			= Application.dataPath;
			DirectoryInfo 	_assetsDirectory 	= new DirectoryInfo(_dataPath + "/");
			FileInfo[]		_fileList			= _assetsDirectory.GetFiles("link.xml", SearchOption.AllDirectories);
			int				_fileCount			= _fileList.Length;

			for (int _iter = 0; _iter < _fileCount; _iter++)
			{
				FileInfo	_curFileInfo		= _fileList[_iter];
				string		_curFilePath		= _curFileInfo.FullName;

				// Load this linker file info
				LoadLinkerFileData(_curFilePath);
			}
		}

		public void SaveLinkerFile ()
		{
			if (m_blacklistedAssemblyList.Count == 0)
			{
				if (FileOperations.Exists(kSavePath))
				{
					FileOperations.Delete(kSavePath);
					FileOperations.Delete(kSavePath + ".meta");
				}

				AssetDatabase.Refresh();
				return;
			}

			// Settings
			XmlWriterSettings 	_settings 	= new XmlWriterSettings();
			_settings.Encoding 				= new System.Text.UTF8Encoding(true);
			_settings.ConformanceLevel 		= ConformanceLevel.Document;
			_settings.OmitXmlDeclaration 	= true;
			_settings.Indent 				= true;

			// Generate and write manifest
			using (XmlWriter _xmlWriter = XmlWriter.Create(kSavePath, _settings))
			{
				_xmlWriter.WriteStartDocument();
				{
					// Write blacklisted classes info
					_xmlWriter.WriteStartElement("linker");
					{
						foreach (BlacklistedAssembly _blacklistedAssembly in m_blacklistedAssemblyList)
						{
							// Write assembly info
							_xmlWriter.WriteStartElement("assembly");
							{
								_xmlWriter.WriteAttributeString("fullname", _blacklistedAssembly.Name);
								WriteStrippingInfo(_xmlWriter, "type",		_blacklistedAssembly.TypeList);
								WriteStrippingInfo(_xmlWriter, "namespace",	_blacklistedAssembly.NamespaceList);
							}
							_xmlWriter.WriteEndElement();
						}
					}
					_xmlWriter.WriteEndElement();
				}
				_xmlWriter.WriteEndDocument();
			}
			
			// Refresh assetdatabase
			AssetDatabase.Refresh();
		}

		private void WriteStrippingInfo (XmlWriter _xmlWriter, string _element, List<string> _entryList)
		{
			foreach (string _entry in _entryList)
			{
				_xmlWriter.WriteStartElement(_element);
				{
					_xmlWriter.WriteAttributeString("fullname", _entry);
					_xmlWriter.WriteAttributeString("preserve", "all");
				}
				_xmlWriter.WriteEndElement();
			}
		}

		private BlacklistedAssembly GetBlacklistedAssembly (string _assemblyName)
		{
			BlacklistedAssembly		_assembly	= m_blacklistedAssemblyList.FirstOrDefault(_curAssembly => _curAssembly.Name.Equals(_assemblyName));
			
			if (_assembly == null)
			{
				_assembly						= new BlacklistedAssembly(_assemblyName);
				
				// Add it to the list
				m_blacklistedAssemblyList.Add(_assembly);
			}
			
			return _assembly;
		}

		#endregion
	}
}
#endif