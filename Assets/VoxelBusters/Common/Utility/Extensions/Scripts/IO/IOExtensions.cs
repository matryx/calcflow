using UnityEngine;
using System.Collections;
using System;
using System.IO;

namespace VoxelBusters.Utility
{
	public static class IOExtensions 
	{
		public static string MakeRelativePath (this string _fromPath, string _toPath)
		{
			if (string.IsNullOrEmpty(_fromPath)) 
				throw new ArgumentNullException("_fromPath");

			if (string.IsNullOrEmpty(_toPath))   
				throw new ArgumentNullException("_toPath");

			return MakeRelativePath(new Uri(_fromPath), _toPath);
		}

		public static string MakeRelativePath (this Uri _fromUri, string _toPath)
		{
#if !NETFX_CORE
			if (_fromUri == null)
				throw new ArgumentNullException("_fromUri");

			Uri 	_toUri 			= new Uri(_toPath);
			
			// Path can't be made relative.
			if (_fromUri.Scheme != _toUri.Scheme) 
				return _toPath;
			
			Uri 	_relativeUri 	= _fromUri.MakeRelativeUri(_toUri);
			string 	_relativePath 	= Uri.UnescapeDataString(_relativeUri.ToString());
			
			if (_toUri.Scheme.ToUpperInvariant() == "_curFile")
				_relativePath = _relativePath.Replace(Path.AltDirectorySeparatorChar, Path.DirectorySeparatorChar);

			return _relativePath;
#else
			return null;
#endif
		}

		public static bool AssignPermissionRecursively (string _directoryPath, FileAttributes _attribute)
		{
			DirectoryInfo	_directoryInfo	= new DirectoryInfo(_directoryPath);

			return AssignPermissionRecursively(_directoryInfo, _attribute);
		}

		public static bool AssignPermissionRecursively (DirectoryInfo _directoryInfo, FileAttributes _attribute)
		{
#if !(UNITY_WEBPLAYER || UNITY_WEBGL || NETFX_CORE)
			if (_directoryInfo == null)
				return false;

			// Update directory attribute
			_directoryInfo.Attributes	|= _attribute;

			// Update file attribute
			foreach (FileInfo _curFileInfo in _directoryInfo.GetFiles())
				_curFileInfo.Attributes |= _attribute;

			// Dig deep into subfolders
			foreach (DirectoryInfo _subDirectoryInfo in _directoryInfo.GetDirectories())
				AssignPermissionRecursively(_subDirectoryInfo, _attribute);

			return true;
#else
			return false;
#endif
		}

		public static void CopyFilesRecursively (string _sourceDirectory, string _destinationDirectory, bool _excludeMetaFiles = true, bool _deleteDestinationFolderIfExists = true) 
		{
#if !(UNITY_WEBPLAYER || UNITY_WEBGL || NETFX_CORE)
			// Get the subdirectories for the specified directory.
			DirectoryInfo 	_sourceDirectoryInfo 		= new DirectoryInfo(_sourceDirectory);
			DirectoryInfo 	_destinationDirectoryInfo 	= new DirectoryInfo(_destinationDirectory);

			CopyFilesRecursively(_sourceDirectoryInfo, _destinationDirectoryInfo, _excludeMetaFiles, _deleteDestinationFolderIfExists);
#else
			Debug.LogError("IOExtensions] Copy files not supported on web player");			
#endif
		}

		public static void CopyFilesRecursively (DirectoryInfo _sourceDirectoryInfo, DirectoryInfo _destinationDirectoryInfo, bool _excludeMetaFiles = true, bool _deleteDestinationFolderIfExists = true)
		{
#if !(UNITY_WEBPLAYER || UNITY_WEBGL || NETFX_CORE)
			if (!_sourceDirectoryInfo.Exists)
				throw new DirectoryNotFoundException(string.Format("Source directory does not exist or could not be found= {0}.", _sourceDirectoryInfo.FullName));

			// Remove existing directory and create new folder
			if (_deleteDestinationFolderIfExists && _destinationDirectoryInfo.Exists)
				_destinationDirectoryInfo.Delete(true);

			_destinationDirectoryInfo.Create();

			// Get the files in the directory and copy them to the new location.
			FileInfo[] 	_files	= _sourceDirectoryInfo.GetFiles();

			if (_excludeMetaFiles)
			{
				foreach (FileInfo _curFileInfo in _files)
				{
					if (_curFileInfo.Extension == ".meta")
						continue;

					CopyFile(_curFileInfo, Path.Combine(_destinationDirectoryInfo.FullName, _curFileInfo.Name));
				}
			}
			else
			{
				foreach (FileInfo _curFileInfo in _files)
					CopyFile(_curFileInfo, Path.Combine(_destinationDirectoryInfo.FullName, _curFileInfo.Name));
			}
			
			// If copying subdirectories, copy them and their contents to new location. 
			DirectoryInfo[]	 _subDirectories = _sourceDirectoryInfo.GetDirectories();

			foreach (DirectoryInfo _subDirectoryInfo in _subDirectories)
				CopyFilesRecursively(_subDirectoryInfo, new DirectoryInfo(Path.Combine(_destinationDirectoryInfo.FullName, _subDirectoryInfo.Name)), _excludeMetaFiles);
#else
			Debug.LogError("IOExtensions] Copy files not supported on web player");
#endif
		}
		
		public static void CopyFile (string _sourceFilePath, string _destinationFilePath, bool _overwrite = true)
		{
			FileInfo	_sourceFileInfo		= new FileInfo(_sourceFilePath);

			if (!_sourceFileInfo.Exists)
				return;

			CopyFile(_sourceFileInfo, _destinationFilePath, _overwrite);
		}

		public static void CopyFile (FileInfo _sourceFileInfo, string _destinationFilePath, bool _overwrite = true)
		{
#if !(UNITY_WEBPLAYER || UNITY_WEBGL || NETFX_CORE)
			// Set attributes to normal, to avoid i/o exceptions
			FileAttributes	_prevAttributes	= _sourceFileInfo.Attributes;
			_sourceFileInfo.Attributes		= FileAttributes.Normal;
			
			// Copy file
			_sourceFileInfo.CopyTo(_destinationFilePath, _overwrite);
			
			// Reset file attribute
			_sourceFileInfo.Attributes		= _prevAttributes;
#else
			Debug.LogError("IOExtensions] Copy files not supported on web player");
#endif
		}
	}
}