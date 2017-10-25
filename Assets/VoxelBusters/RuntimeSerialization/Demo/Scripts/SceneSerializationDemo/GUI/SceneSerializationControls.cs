using UnityEngine;
using System.Collections;
using VoxelBusters.AssetStoreProductUtility.Demo;

namespace VoxelBusters.RuntimeSerialization.Demo
{
	public class SceneSerializationControls : DemoGUIWindow 
	{
		#region Properties

		[SerializeField]
		private				PrimitiveGenerator		m_primitiveGenerator;

		#endregion
	
		#region Methods

		protected override void OnGUIWindow ()
		{
			base.OnGUIWindow ();

			// Window background not required
			GUI.skin.window.normal.background		= null;
			GUI.skin.window.active.background		= null;
			GUI.skin.window.focused.background		= null;
			GUI.skin.window.hover.background		= null;
			GUI.skin.window.onNormal.background		= null;
			GUI.skin.window.onActive.background		= null;
			GUI.skin.window.onFocused.background	= null;
			GUI.skin.window.onHover.background		= null;

			// Properties for buttons
			GUILayoutOption _minWidth				= GUILayout.MinWidth(Screen.width * 0.2f);
			GUILayoutOption _expHeight				= GUILayout.ExpandHeight(true);

			GUILayout.BeginArea(new Rect(0, 0, Screen.width, Screen.height));
			GUILayout.FlexibleSpace();
			GUILayout.BeginHorizontal();
			{
				GUILayout.FlexibleSpace();

				if (GUILayout.Button("Spawn", 		_minWidth, _expHeight))
				{
					m_primitiveGenerator.SpawnNewObject();
				}

				if (GUILayout.Button("Serialize", 	_minWidth, _expHeight))
				{
					m_primitiveGenerator.Serialize();
				}

				if (GUILayout.Button("Clear", 		_minWidth, _expHeight))
				{
					m_primitiveGenerator.Clear();
				}
				
				if (GUILayout.Button("Deserialize",	_minWidth, _expHeight))
				{
					m_primitiveGenerator.Deserialize();
				}

				if (GUILayout.Button("Main Menu",	_minWidth, _expHeight))
				{
					Application.LoadLevel("SerializationDemo");
				}

				GUILayout.FlexibleSpace();
			}
			GUILayout.EndHorizontal();
			GUILayout.Space(30f);
			GUILayout.EndArea();
		}

		#endregion
	}
}