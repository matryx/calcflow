using UnityEngine;
using System.Collections;

#if UNITY_EDITOR
using UnityEditor;

namespace VoxelBusters.Utility
{
	public partial class ShaderUtility : AdvancedScriptableObject <ShaderUtility>
	{
		#region Menu methods

		private		const	string		kReloadShaderUtilityMenuPath			= "Window/Voxel Busters/Misc./Reload Shader Utility";

		[MenuItem(kReloadShaderUtilityMenuPath, false, 10000)]
		private static void OnPressingReloadShaderUtility ()
		{
			// Recollect info
			ShaderUtility.Instance.ReloadShaderUtility();

			// Select asset
			Selection.activeObject	= ShaderUtility.Instance;
		}

		#endregion
	}
}
#endif