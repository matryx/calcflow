using UnityEngine;
using System.Collections;
using System;
using System.Collections.Generic;

#if UNITY_EDITOR
using System.IO;
using UnityEditor;
#endif

namespace VoxelBusters.Utility
{
	public partial class ShaderUtility : AdvancedScriptableObject <ShaderUtility>
	{
		#region Properties

		[SerializeField]
		private				List<ShaderInfo>		m_shaderInfoList			= new List<ShaderInfo>();
		public				List<ShaderInfo>		ShaderInfoList
		{
			get
			{
				return m_shaderInfoList;
			}

			private set
			{
				m_shaderInfoList	= value;
			}
		}

#pragma warning disable
		private				string[]				m_builtInAssetPathList 	= new string[]{
																					"Resources/unity_builtin_extra", 
																		         	"Library/unity default resources"
																				};
#pragma warning restore

		#endregion

		#region Methods

		protected override void OnEnable ()
		{
			base.OnEnable ();
			
			if (m_shaderInfoList == null)
				m_shaderInfoList	= new List<ShaderInfo>();
		}

		public ShaderInfo GetShaderInfo (Material _material)
		{
			if (_material == null)
				return null;

			Shader	_shader		= _material.shader;

			if (_shader == null)
				return null;

			return GetShaderInfo(_shader.name);
		}

		public ShaderInfo GetShaderInfo (string _shaderName)
		{
			for (int _iter = 0; _iter < m_shaderInfoList.Count; _iter++)
			{
				ShaderInfo	_curShaderInfo	= m_shaderInfoList[_iter];

				if (_shaderName.Equals(_curShaderInfo.Name))
					return _curShaderInfo;
			}

			return null;
		}
		
#if !UNITY_EDITOR
		public void ReloadShaderUtility ()
		{}
#else
		public void ReloadShaderUtility ()
		{
			if (Application.isPlaying)
				return;
			
			// Clear existing entries
			m_shaderInfoList.Clear();
			
			// Find inbuilt shaders 
			FindAllBuiltInShaders(ref m_shaderInfoList);
			
			// Find custom shaders
			FindAllCustomShaders(ref m_shaderInfoList);

			// Forcing unity to serialize
			EditorUtility.SetDirty(this);
		}

		private void  FindAllBuiltInShaders (ref List<ShaderInfo> _shaderInfoList)
		{
			int			_count		= m_builtInAssetPathList.Length;

			for (int _iter = 0; _iter < _count; _iter++)
			{
				string	_curPath	= m_builtInAssetPathList[_iter];

				if (_curPath != null)
					FindAllBuiltInShaders(_curPath, ref _shaderInfoList);
			}
		}

		private void  FindAllBuiltInShaders (string _builtInFolderPath, ref List<ShaderInfo> _shaderInfoList)
		{
			UnityEngine.Object[] 	_assetList  	= AssetDatabase.LoadAllAssetsAtPath(_builtInFolderPath) as UnityEngine.Object[];

			if (_assetList == null)
				return;

			int						_assetCount		= _assetList.Length;

			for (int _iter = 0; _iter < _assetCount; _iter++)
			{
				UnityEngine.Object	_curAsset		= _assetList[_iter];

				if (_curAsset.GetType() == typeof(Shader))
					_shaderInfoList.Add(new ShaderInfo(_curAsset as Shader));
			}
		}

		private void  FindAllCustomShaders (ref List<ShaderInfo> _shaderInfoList)
		{
			string[] 		_assetPaths		= AssetDatabase.GetAllAssetPaths();
			int				_pathCount		= _assetPaths.Length;
				
			for (int _iter = 0; _iter < _pathCount; _iter++)
			{
				string		_curAssetPath	= _assetPaths[_iter];
				string		_fileExtension	= Path.GetExtension(_curAssetPath);

				if (!_fileExtension.Equals(".shader"))
					continue;

				Shader		_shader			= AssetDatabase.LoadAssetAtPath(_curAssetPath, typeof(Shader)) as Shader;

				if (_shader == null)
					continue;

				// Add this shader
				_shaderInfoList.Add(new ShaderInfo(_shader));
			}
		}
#endif

		#endregion
	}
}