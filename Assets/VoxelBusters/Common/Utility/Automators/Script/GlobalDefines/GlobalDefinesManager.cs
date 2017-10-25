using UnityEngine;
using System.Collections;
using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

#if UNITY_EDITOR && !(UNITY_WEBPLAYER || UNITY_WEBGL)
using UnityEditor;

namespace VoxelBusters.Utility
{
	public class GlobalDefinesManager 
	{
		public enum eCompiler
		{
			CSHARP,
			EDITOR,
			UNITY_SCRIPT,
			BOO
		}

		#region Properties

		public				List<string> 	CSharpDefinesList
		{
			get;
			private set;
		}

		public				List<string> 	BooDefinesList
		{
			get;
			private set;
		}

		public				List<string> 	UnityScriptDefinesList
		{
			get;
			private set;
		}

		public				List<string> 	EditorDefinesList
		{
			get;
			private set;
		}

		private		static	string			reimportFilePath		= null;

		#endregion

		#region Constants

		// Path
		private		const 	string 			kCSharpFilePath 		= "Assets/smcs.rsp";
		private		const 	string 			kEditorFilePath 		= "Assets/gmcs.rsp";
		private		const 	string 			kBooFilePath 			= "Assets/boo.rsp";
		private		const 	string 			kUnityScriptFilePath 	= "Assets/us.rsp";

		// Defines
		private		const 	string			kDefinePrefix			= "-define:";

		#endregion

		#region Constructors

		public GlobalDefinesManager ()
		{
			// Initialize 
			CSharpDefinesList			= new List<string>();
			BooDefinesList				= new List<string>();
			UnityScriptDefinesList		= new List<string>();
			EditorDefinesList			= new List<string>();

			// Load defines
			LoadAllDefines();
		}

		#endregion

		#region Methods

		private void LoadAllDefines ()
		{
			CSharpDefinesList 			= LoadDefines(kCSharpFilePath);
			UnityScriptDefinesList 		= LoadDefines(kUnityScriptFilePath);
			BooDefinesList 				= LoadDefines(kBooFilePath);
			EditorDefinesList		 	= LoadDefines(kEditorFilePath);
		}

		public void SaveAllCompilers ()
		{
			SaveCompiler(eCompiler.CSHARP, 			false);
			SaveCompiler(eCompiler.UNITY_SCRIPT, 	false);
			SaveCompiler(eCompiler.BOO, 			false);
			SaveCompiler(eCompiler.EDITOR, 			false);
			
			// Reimport
			ReimportToForceCompile();
		}

		public void SaveCompiler (eCompiler _compiler, bool _reimport = true)
		{
			string			_path			= GetSavePath(_compiler);
			List<string> 	_definesList	= GetDefinesList(_compiler);
			
			// Write defines
			if (_definesList.Count == 0)
			{
				if (FileOperations.Exists(_path))
				{
					FileOperations.Delete(_path);
					FileOperations.Delete(_path + ".meta");
				}

				AssetDatabase.Refresh();
				return;
			}

			StringBuilder 	_stringBuilder 	= new StringBuilder();

			_stringBuilder.Append(kDefinePrefix);
			_stringBuilder.Append(string.Join(";", _definesList.ToArray()));
			
			using (StreamWriter _writer = new StreamWriter(_path, false))
				_writer.Write(_stringBuilder.ToString());

			// Reimport
			if (_reimport)
				ReimportToForceCompile();
		}

		public List<string> GetDefinesList (eCompiler _compiler)
		{
			switch (_compiler)
			{
			case  eCompiler.BOO:
				return  BooDefinesList;
				
			case eCompiler.CSHARP:
				return CSharpDefinesList;
				
			case eCompiler.EDITOR:
				return EditorDefinesList;
				
			case eCompiler.UNITY_SCRIPT:
				return UnityScriptDefinesList;
			}

			return null;
		}

		private string GetSavePath (eCompiler _compiler)
		{
			switch (_compiler)
			{
			case  eCompiler.BOO:
				return  kBooFilePath;
				
			case eCompiler.CSHARP:
				return kCSharpFilePath;
				
			case eCompiler.EDITOR:
				return kEditorFilePath;
				
			case eCompiler.UNITY_SCRIPT:
				return kUnityScriptFilePath;
			}
			
			return null;
		}

		public void AddNewDefineSymbol (eCompiler _compiler, string _newDefineSymbol)
		{
			List<string> 	_definesList	= GetDefinesList(_compiler);

			if (!_definesList.Contains(_newDefineSymbol))
				_definesList.Add(_newDefineSymbol);
		}

		public void RemoveDefineSymbol (eCompiler _compiler, string _defineSymbol)
		{
			GetDefinesList(_compiler).Remove(_defineSymbol);
		}

		public void SetAllCompilerDefines (eCompiler _compiler)
		{
			List<string> 	_definesList	= GetDefinesList(_compiler);

			// Set new defines list
			CSharpDefinesList				= new List<string>(_definesList);
			BooDefinesList					= new List<string>(_definesList);
			UnityScriptDefinesList			= new List<string>(_definesList);
			EditorDefinesList				= new List<string>(_definesList);
		}

		private List<string> LoadDefines (string _path)
		{
			if (!FileOperations.Exists(_path))
				return new List<string>();
			
			string 			_fileContents 	= FileOperations.ReadAllText(_path);
			int				_prefixLength	= kDefinePrefix.Length;

			if (_fileContents.StartsWith(kDefinePrefix))
				_fileContents				= _fileContents.Substring(_prefixLength);

			string[]		_tokenList		= _fileContents.Split(';');

			return new List<string>(_tokenList);
		}

		private void ReimportToForceCompile ()
		{
			// Refesh 
			AssetDatabase.Refresh();

			// Reimport file to ensure changes are applied
			if (reimportFilePath == null)
			{
				string			_dataPath			= Application.dataPath;
				DirectoryInfo 	_assetsPathDir 		= new DirectoryInfo(_dataPath);
				Uri				_assetsPathURI		= new Uri(_dataPath);

				foreach (FileInfo _currentFile in _assetsPathDir.GetFiles("GlobalDefinesManager.cs", SearchOption.AllDirectories))
				{
					Uri 		_relativePathURI 	= _assetsPathURI.MakeRelativeUri(new Uri(_currentFile.FullName));
					string 		_relativePath		= Uri.UnescapeDataString(_relativePathURI.ToString());

					// Cache path of the file to be reimported on save
					reimportFilePath				= _relativePath;
					break;
				}
			}

			AssetDatabase.ImportAsset(reimportFilePath, ImportAssetOptions.ForceUpdate);
		}

		#endregion
	}
}
#endif