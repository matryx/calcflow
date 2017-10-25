using UnityEngine;
using System.Collections;
using VoxelBusters.Utility;

namespace VoxelBusters.AssetStoreProductUtility.Internal
{
	internal struct ProductUpdateInfo
	{
		#region Constants
		
		private const 	string 				kVersionNumberKey		= "version_number";
		private const 	string 				kDownloadLinkKey		= "download_link";
		private const 	string				kAssetStoreLink			= "asset_store_link";
		private const 	string				kReleaseNoteKey			= "release_notes";
		
		#endregion
		
		#region Properties
		
		internal 		bool				NewUpdateAvailable
		{
			get;
			private set;
		}
		
		internal 		string				VersionNumber
		{
			get;
			private set;
		}
		
		internal		string				DownloadLink
		{
			get;
			private set;
		}
		
		internal		string				AssetStoreLink
		{
			get;
			private set;
		}
		
		internal		string				ReleaseNote
		{
			get;
			private set;
		}
		
		#endregion
		
		#region Constructor
		
		internal ProductUpdateInfo (bool _newUpdateAvailable) : this ()
		{
			NewUpdateAvailable			= _newUpdateAvailable;
			VersionNumber				= null;
			DownloadLink				= null;
			AssetStoreLink				= null;
			ReleaseNote					= null;
		}
		
		internal ProductUpdateInfo (string _currentVersion, IDictionary _dataDict) : this ()
		{
			string _availableVersion	= _dataDict.GetIfAvailable<string>(kVersionNumberKey);
			string _downloadLink		= _dataDict.GetIfAvailable<string>(kDownloadLinkKey);
			string _assetStoreLink		= _dataDict.GetIfAvailable<string>(kAssetStoreLink);
			string _releaseNote			= _dataDict.GetIfAvailable<string>(kReleaseNoteKey);
			
			// Update class properties
			NewUpdateAvailable			= (_availableVersion.CompareTo(_currentVersion) > 0);
			VersionNumber				= _availableVersion;
			DownloadLink				= _downloadLink;
			AssetStoreLink				= _assetStoreLink;
			ReleaseNote					= _releaseNote;
		}
		
		#endregion
	}
}