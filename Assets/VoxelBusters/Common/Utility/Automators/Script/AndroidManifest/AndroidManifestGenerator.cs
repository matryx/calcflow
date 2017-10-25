using UnityEngine;
using System.Collections;

#if UNITY_EDITOR
using System.Xml;

namespace VoxelBusters.Utility
{
	public partial class AndroidManifestGenerator 
	{
		#region Methods
		
		public void SaveManifest (string _packageName, string _path)
		{
			SaveManifest(_packageName, "1", "1.0", _path);
		}
		
		public void SaveManifest (string _packageName, string _versionCode, string _versionName, string _path)
		{
			// Settings
			XmlWriterSettings _settings 	= new XmlWriterSettings();
			_settings.Encoding 				= new System.Text.UTF8Encoding(true);
			_settings.ConformanceLevel 		= ConformanceLevel.Document;
			_settings.Indent 				= true;
			
			// Generate and write manifest
			using (XmlWriter _xmlWriter = XmlWriter.Create(_path, _settings))
			{
				_xmlWriter.WriteStartDocument();
				{
					//********************
					// Manifest
					//********************
					_xmlWriter.WriteComment("AUTO GENERATED MANIFEST FILE FROM AndroidManifestGenerator. DONT MODIFY HERE.");
					
					_xmlWriter.WriteStartElement("manifest");					
					_xmlWriter.WriteAttributeString("xmlns:android", 		"http://schemas.android.com/apk/res/android");
					_xmlWriter.WriteAttributeString("package", 				_packageName);
					_xmlWriter.WriteAttributeString("android:versionCode", 	_versionCode);
					_xmlWriter.WriteAttributeString("android:versionName", 	_versionName);
					{
						//********************
						// Application
						//********************
						_xmlWriter.WriteStartElement("application");
						{
							WriteApplicationProperties(_xmlWriter);
						}
						_xmlWriter.WriteEndElement();
						
						//********************
						// Permission
						//********************
						_xmlWriter.WriteComment("Permissions");
						WritePermissions(_xmlWriter);
					}
					_xmlWriter.WriteEndElement();
				}
				_xmlWriter.WriteEndDocument();
			}
		}
		
		protected virtual void WriteApplicationProperties (XmlWriter _xmlWriter)
		{}
		
		protected virtual void WritePermissions (XmlWriter _xmlWriter)
		{}
		
		protected void WriteActivity (XmlWriter _xmlWriter, string _name, string _theme = null, string _orientation = null, string _configChanges = null, string _comment = null)
		{
			if (_comment != null)
				_xmlWriter.WriteComment(_comment);
			
			_xmlWriter.WriteStartElement("activity");
			{
				_xmlWriter.WriteAttributeString("android:name", 					_name);
				
				if (_theme != null)
					_xmlWriter.WriteAttributeString("android:theme", 				_theme);
				
				if (_orientation != null)
					_xmlWriter.WriteAttributeString("android:screenOrientation", 	_orientation);
				
				if (_configChanges != null)
					_xmlWriter.WriteAttributeString("android:configChanges", 		_configChanges);
			}
			_xmlWriter.WriteEndElement();
		}
		
		protected void WriteAction (XmlWriter _xmlWriter, string _name, string _comment = null)
		{
			if (_comment != null)
				_xmlWriter.WriteComment(_comment);
			
			_xmlWriter.WriteStartElement("action");
			{
				_xmlWriter.WriteAttributeString("android:name", 	_name);
			}
			_xmlWriter.WriteEndElement();
		}
		
		protected void WriteCategory (XmlWriter _xmlWriter, string _name, string _comment = null)
		{
			if (_comment != null)
				_xmlWriter.WriteComment(_comment);
			
			_xmlWriter.WriteStartElement("category");
			{
				_xmlWriter.WriteAttributeString("android:name", 	_name);
			}
			_xmlWriter.WriteEndElement();
		}
		
		protected void WriteService (XmlWriter _xmlWriter, string _name, string _comment = null)
		{
			if (_comment != null)
				_xmlWriter.WriteComment(_comment);
			
			_xmlWriter.WriteStartElement("service");
			{
				_xmlWriter.WriteAttributeString("android:name", 	_name);
			}
			_xmlWriter.WriteEndElement();
		}
		
		protected void WritePermission (XmlWriter _xmlWriter, string _name, string _protectionLevel, string _comment = null)
		{
			if (_comment != null)
				_xmlWriter.WriteComment(_comment);
			
			_xmlWriter.WriteStartElement("permission");
			{
				_xmlWriter.WriteAttributeString("android:name", 			_name);
				_xmlWriter.WriteAttributeString("android:protectionLevel", 	_protectionLevel);
			}
			_xmlWriter.WriteEndElement();
		}
		
		protected void WriteUsesPermission (XmlWriter _xmlWriter, string _name, Feature[] _features = null, string _comment = null)
		{
			if (_comment != null)
				_xmlWriter.WriteComment(_comment);
			
			_xmlWriter.WriteStartElement("uses-permission");
			{
				_xmlWriter.WriteAttributeString("android:name", 			_name);
			}
			_xmlWriter.WriteEndElement();
			
			if (_features != null)
			{
				int				_count		= _features.Length;
				
				for (int _iter = 0; _iter < _count; _iter++)
				{
					Feature		_curFeature	= _features[_iter];
					
					_xmlWriter.WriteStartElement("uses-feature");
					{
						_xmlWriter.WriteAttributeString("android:name", 	_curFeature.Name);
						_xmlWriter.WriteAttributeString("android:required", _curFeature.Required ? "true" : "false");
					}
					_xmlWriter.WriteEndElement();
				}
			}
		}
		
		#endregion
	}
}
#endif