using UnityEngine;
using System.Collections;
using UnityEditor;

namespace VoxelBusters.Utility
{
	public class AdvancedScriptableObjectInspector : Editor 
	{
		#region Fields

		private		float	m_leftMarginOffset	= 10f;
		private		float	m_rightMarginOffset	= 5f;

		#endregion

		#region Unity Methods
		
		protected virtual void OnEnable ()
		{}
		
		protected virtual void OnDisable ()
		{}

		public override bool UseDefaultMargins ()
		{
			return false;
		}
		
		public override void OnInspectorGUI ()
		{
			// Update object
			serializedObject.Update();
			
			// Make all EditorGUI look like regular controls
			EditorGUIUtility.LookLikeControls();

			// Draw inspector
			GUILayout.BeginVertical(EditorStyles.inspectorFullWidthMargins);
			{
				OnGUIWindow();
			}
			GUILayout.EndVertical();

			// Apply modifications
			if (GUI.changed)
				serializedObject.ApplyModifiedProperties();
		}

		#endregion

		#region GUI Methods

		protected virtual void OnGUIWindow ()
		{
			DrawProperties(UnityEditorUtility.kOuterContainerStyle);
		}
	
		#endregion

		#region Methods

		protected void DrawProperties (string _style)
		{
			DrawProperties(new GUIStyle(_style));
		}

		protected void DrawProperties (GUIStyle _style)
		{
			SerializedProperty 	_property	= serializedObject.GetIterator();
			
			// Move to next
			_property.NextVisible(true);

			GUILayout.BeginHorizontal(_style);
			{
				GUILayout.Space(m_leftMarginOffset);
				GUILayout.BeginVertical();
				{
					GUILayout.Space(2f);
					while (_property.NextVisible(false))
						EditorGUILayout.PropertyField(_property, true);
					GUILayout.Space(4f);
				}
				GUILayout.EndVertical();	
				GUILayout.Space(m_rightMarginOffset);
			}
			GUILayout.EndHorizontal();
		}

		protected void DrawProperty (string _propertyName, string _style)
		{
			DrawProperty(_propertyName, new GUIStyle(_style));
		}

		protected void DrawProperty (string _propertyName, GUIStyle _style)
		{
			SerializedProperty 	_property	= serializedObject.FindProperty(_propertyName);

			GUILayout.BeginHorizontal(_style);
			{
				GUILayout.Space(m_leftMarginOffset);
				GUILayout.BeginVertical();
				{
					EditorGUILayout.PropertyField(_property, true);
				}
				GUILayout.EndVertical();	
				GUILayout.Space(m_rightMarginOffset);
			}
			GUILayout.EndHorizontal();
		}

		protected void DrawChildProperties (string _propertyName, GUIStyle _style)
		{
			SerializedProperty 	_property		= serializedObject.FindProperty(_propertyName);
			SerializedProperty 	_endProperty	= _property.GetEndProperty();

			// Move to child property
			_property.NextVisible(true);

			// Draw layout
			GUILayout.BeginHorizontal(_style);
			{
				GUILayout.Space(m_leftMarginOffset);
				GUILayout.BeginVertical();
				{
					do
					{
						if (SerializedProperty.EqualContents(_property, _endProperty))
							break;
						
						EditorGUILayout.PropertyField(_property, true);
					}while (_property.NextVisible(false));
				}
				GUILayout.EndVertical();	
				GUILayout.Space(m_rightMarginOffset);
			}
			GUILayout.EndHorizontal();
		}

		protected void SetLeftMarginOffset (float _newValue)
		{
			m_leftMarginOffset	= _newValue;
		}

		protected void SetRightMarginOffset (float _newValue)
		{
			m_rightMarginOffset	= _newValue;
		}

		#endregion
	}
}