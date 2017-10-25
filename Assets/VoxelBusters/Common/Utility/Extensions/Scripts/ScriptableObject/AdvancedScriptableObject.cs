using UnityEngine;
using System.Collections;
using System.IO;
using System.Text;

#if UNITY_EDITOR
using UnityEditor;
#endif

namespace VoxelBusters.Utility
{
	public class AdvancedScriptableObject <T> : ScriptableObject where T : ScriptableObject
	{
		#region Properties

		private 	static 		T					instance							= null;
		public 		static 		T 					Instance
		{
			get 
			{ 
				if (instance == null)
					instance	= GetAsset(typeof(T).Name);		

				return instance;
			}
		}

		#endregion

		#region Constants

		// Relative path to which asset file will be saved
		private 	const 		string 				kRelativePathToAssetsGroup			= "Assets/Resources/VoxelBusters";

		// Common path info
		private 	const 		string 				kAssetsFolderName					= "Assets";
		private 	const 		string 				kResourcesFolderName				= "Resources";
		private 	const 		string 				kRelativePathToResources			= "Assets/Resources";

		// Group name
		private static readonly	string 				assetGroupFolderName;

		#endregion

		#region Constructors

		static AdvancedScriptableObject ()
		{
			string				_unrootedRelativePath	= kRelativePathToAssetsGroup.TrimStart('/');
			string[]			_relativePathComponents	= _unrootedRelativePath.Split('/');
			int					_pathComponentCount		= _relativePathComponents.Length;

			if (_pathComponentCount < 2)
			{
				Debug.LogError("[AdvancedScriptableObject] Invalid relative path to asset. Please fix it.");
				return;
			}

			// Check if grouping is specified or not
			if (_pathComponentCount == 2)
			{
				assetGroupFolderName					= null;
			}
			else				
			{
				StringBuilder	_stringBuilder			= new StringBuilder();

				for (int _iter = 2; _iter < _pathComponentCount; _iter++)
				{
					if (_iter != 2)
						_stringBuilder.Append('/');

					_stringBuilder.Append(_relativePathComponents[_iter]);
				}

				// Assign group folder name
				assetGroupFolderName					= _stringBuilder.ToString();
			}
		}

		#endregion

		#region Methods

		protected virtual void Reset ()
		{}

		protected virtual void OnEnable ()
		{
			if (instance == null)
				instance	= this as T;
		}

		protected virtual void OnDisable ()
		{}

		protected virtual void OnDestroy ()
		{}

		public void Save ()
		{
#if UNITY_EDITOR
			if (EditorApplication.isPlaying || EditorApplication.isPaused)
				return;

			EditorUtility.SetDirty(this);

			// Save
			AssetDatabase.SaveAssets();

			// Refresh
			AssetDatabase.Refresh();
#endif
		}

		#endregion

		#region Static Methods

		public static T GetAsset (string _assetName)
		{
#if !UNITY_EDITOR
			if (assetGroupFolderName == null)
				return Resources.Load<T>(_assetName);
			else
				return Resources.Load<T>(string.Format("{0}/{1}", assetGroupFolderName, _assetName));
#else
			string 		_assetSavedAtPath				= string.Format("{0}/{1}.asset", kRelativePathToAssetsGroup, _assetName);
			T 			_scriptableObject 				= AssetDatabase.LoadAssetAtPath(_assetSavedAtPath, typeof(T)) as T;

			if (_scriptableObject == null)
			{
				// Need to create folder "Assets/Resources", if it doesnt exist
				if (!AssetsUtility.FolderExists(kRelativePathToResources))
				{
					AssetDatabase.CreateFolder(kAssetsFolderName, kResourcesFolderName);
				}

				// Need to create folder "Assets/Resources/Group", if it doesnt exist
				if (!AssetsUtility.FolderExists(kRelativePathToAssetsGroup))
				{
					AssetDatabase.CreateFolder(kRelativePathToResources, assetGroupFolderName);
				}
				
				// Refresh
				AssetDatabase.Refresh();
				
				// Create asset
				T 		_newAsset						= ScriptableObject.CreateInstance<T>();

				AssetDatabase.CreateAsset(_newAsset, AssetDatabase.GenerateUniqueAssetPath(_assetSavedAtPath));

				// Save
				(_newAsset as AdvancedScriptableObject<T>).Save();

				// Assign reference of newly created asset
				_scriptableObject						= _newAsset;
			}
			
			return _scriptableObject;
#endif
		}

		#endregion
	}
}