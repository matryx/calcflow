using UnityEngine;
using System.Collections;
using System.IO;

namespace VoxelBusters.ThirdParty.XUPorter 
{
	public class XCMod 
	{
		private Hashtable _datastore = new Hashtable();
		private ArrayList _libs = null;
		
		public string name { get; private set; }
		public string path { get; private set; }
		
		public string group {
			get {
				if (_datastore != null && _datastore.Contains("group"))
					return (string)_datastore["group"];
				return string.Empty;
			}
		}
		
		public ArrayList patches {
			get {
				if (_datastore.Contains("patches")) {
					return (ArrayList)_datastore["patches"];
				}

				return new ArrayList();
			}
		}
		
		public ArrayList libs {
			get {
				if( _libs == null ) 
				{
					if (_datastore.Contains("libs")) {
						_libs	= new ArrayList( ((ArrayList)_datastore["libs"]).Count );
						
						foreach( string fileRef in (ArrayList)_datastore["libs"] ) {
							Debug.Log("Adding to Libs: "+fileRef);
							_libs.Add( new XCModFile( fileRef ) );
						}
					}
					else {
						_libs	= new ArrayList();
					}
				}
				return _libs;
			}
		}
		
		public ArrayList frameworks {
			get {
				if (_datastore.Contains("frameworks")) {
					return (ArrayList)_datastore["frameworks"];
				}

				return new ArrayList();
			}
		}
		
		public ArrayList headerpaths {
			get {
				if (_datastore.Contains("headerpaths")) {
					return (ArrayList)_datastore["headerpaths"];
				}
				
				return new ArrayList();
			}
		}
		
		public ArrayList files {
			get {
				if (_datastore.Contains("files")) {
					return (ArrayList)_datastore["files"];
				}
				
				return new ArrayList();
			}
		}
		
		public ArrayList folders {
			get {
				if (_datastore.Contains("folders")) {
					return (ArrayList)_datastore["folders"];
				}
				
				return new ArrayList();
			}
		}
		
		public ArrayList excludes {
			get {
				if (_datastore.Contains("excludes")) {
					return (ArrayList)_datastore["excludes"];
				}
				
				return new ArrayList();
			}
		}

		public ArrayList compiler_flags {
			get {
				if (_datastore.Contains("compiler_flags")) {
					return (ArrayList)_datastore["compiler_flags"];
				}
				
				return new ArrayList();
			}
		}

		public ArrayList linker_flags {
			get {
				if (_datastore.Contains("linker_flags")) {
					return (ArrayList)_datastore["linker_flags"];
				}
				
				return new ArrayList();
			}
		}
		
		public XCMod( string filename )
		{	
			FileInfo projectFileInfo = new FileInfo( filename );
			if( !projectFileInfo.Exists ) {
				Debug.LogWarning( "File does not exist." );
			}
			
			name = System.IO.Path.GetFileNameWithoutExtension( filename );
			path = System.IO.Path.GetDirectoryName( filename );
			
			string contents = projectFileInfo.OpenText().ReadToEnd();
			Debug.Log (contents);
			_datastore = (Hashtable)XUPorter.MiniJSON.jsonDecode( contents );
			if (_datastore == null || _datastore.Count == 0) {
				Debug.Log (contents);
				throw new UnityException("Parse error in file " + System.IO.Path.GetFileName(filename) + "! Check for typos such as unbalanced quotation marks, etc.");
			}
		}
	}

	public class XCModFile
	{
		public string filePath { get; private set; }
		public bool isWeak { get; private set; }
		
		public XCModFile( string inputString )
		{
			isWeak = false;
			
			if( inputString.Contains( ":" ) ) {
				string[] parts = inputString.Split( ':' );
				filePath = parts[0];
				isWeak = ( parts[1].CompareTo( "weak" ) == 0 );	
			}
			else {
				filePath = inputString;
			}
		}
	}
}
