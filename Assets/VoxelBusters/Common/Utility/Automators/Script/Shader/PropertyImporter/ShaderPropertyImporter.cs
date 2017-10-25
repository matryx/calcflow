using UnityEngine;
using System.Collections;

#if UNITY_EDITOR
using UnityEditor;

namespace VoxelBusters.Utility
{
	public class ShaderPropertyImporter : AssetPostprocessor 
	{
		#region Static Methods
		
#if !DISABLE_SHADER_PROPERTY_IMPORTER
		private static void OnPostprocessAllAssets (string[] importedAssets, string[] deletedAssets, string[] movedAssets, string[] movedFromAssetPaths) 
		{
			ShaderUtility	_instance	= ShaderUtility.Instance;
			
			// Reloads shader information
			_instance.ReloadShaderUtility();
		}
#endif
		#endregion
	}
}
#endif