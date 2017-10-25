using UnityEngine;
using System.Collections;
using UnityEditor;
using VoxelBusters.Utility;

namespace VoxelBusters.RuntimeSerialization
{
	[CustomEditor(typeof(UIDSystem))]
	public class UIDSystemInspector : Editor 
	{
		#region Methods
		
		public override void OnInspectorGUI ()
		{
			DrawDefaultInspector();

			if (IsEditMode())
			{
				GUILayout.BeginHorizontal();
				{
					GUILayout.FlexibleSpace();
					
					if (GUILayout.Button("Update UIDSystem", GUILayout.MinWidth(120f), GUILayout.MinHeight(22f)))
					{
						Undo.RecordObject(target, "Update UIDSystem");
						target.InvokeMethod(UIDSystem.kMethodUpdateUIDs, true);
						EditorUtility.SetDirty(target);
					}

					GUILayout.Space(20f);

					if (GUILayout.Button("Reassign UIDs", GUILayout.MinWidth(120f), GUILayout.MinHeight(22f)))
					{
						Undo.RecordObject(target, "Reassign UIDs");
						target.InvokeMethod(UIDSystem.kMethodReassignUIDs, true);
						EditorUtility.SetDirty(target);
					}

					GUILayout.FlexibleSpace();
				}
				GUILayout.EndHorizontal();
			}
		}

		private bool IsEditMode ()
		{
			return !(EditorApplication.isPlaying || EditorApplication.isPaused || EditorApplication.isPlayingOrWillChangePlaymode);
		}
		
		#endregion
	}
}