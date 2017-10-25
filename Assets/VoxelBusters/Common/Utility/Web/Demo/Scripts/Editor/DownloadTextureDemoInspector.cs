using UnityEngine;
using UnityEditor;
using System.Collections;

namespace VoxelBusters.Utility
{
	[CustomEditor(typeof(DownloadTextureDemo))]
	public class DownloadTextureDemoInspector : Editor 
	{
		#region Unity Methods

		public override void OnInspectorGUI ()
		{
			DrawDefaultInspector();

			if (GUILayout.Button("StartDownload"))
			{
				(target as DownloadTextureDemo).StartDownload();
			}
		}

		#endregion
	}
}
