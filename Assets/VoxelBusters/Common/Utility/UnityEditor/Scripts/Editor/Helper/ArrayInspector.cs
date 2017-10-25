using UnityEngine;
using UnityEditor;
using System.Collections;

namespace VoxelBusters.Utility
{
	public class ArrayInspector 
	{
		#region Properties

		public enum eArrayOptions
		{
			DEFAULT					= 0,
			SHOW_NAME_WITH_FOLDOUT	= 0x10,
			SHOW_ARRAY_SIZE			= 0x100,
		}

		// Constants
		private const float kButtonHeight			= 20f;

		#endregion

		#region Inspector Methods

		public static void Draw (SerializedProperty _arrayProperty, GUIContent _label, eArrayOptions _options)
		{
			bool _showNameWithFoldout	= (_options & eArrayOptions.SHOW_NAME_WITH_FOLDOUT) != 0;
			bool _showArraySize			= (_options & eArrayOptions.SHOW_ARRAY_SIZE) != 0;
			int _count					= _arrayProperty.arraySize;
			int _originalIndentLevel	= EditorGUI.indentLevel;

			// GUI styles
			GUIStyle _buttonStyle		= EditorStyles.miniButton;
			
			// Show array name
			if (_showNameWithFoldout)
			{
				_arrayProperty.isExpanded = EditorGUILayout.Foldout(_arrayProperty.isExpanded, _label);

				// Update indentation
				EditorGUI.indentLevel++;
			}
			else
			{
				EditorGUILayout.LabelField(_label);
			}

			// Is foldout enabled, then dont show the rest of the elements
			if (!_arrayProperty.isExpanded)
			{
				// Reset indentation to original value
				EditorGUI.indentLevel	= _originalIndentLevel;
				return;
			}

			// Show array size
			if (_showArraySize && _count != 0)
			{
				// Check if size value changes
				EditorGUI.BeginChangeCheck();
				int _newSize		= EditorGUILayout.IntField("Size", _count);
				
				if (EditorGUI.EndChangeCheck())
					_arrayProperty.arraySize	= _newSize;
			}
			
			// If there are no elements then return size of add button
			if (_count == 0)
			{
				if (GUILayout.Button("Add new product", GUILayout.Height(kButtonHeight)))
					_arrayProperty.InsertArrayElementAtIndex(0);
			}
			else
			{
				// Show array elements if it is expanded
				GUIStyle _subviewStyle		= new GUIStyle("ProgressBarBack");

				if (_arrayProperty.isExpanded) 
				{
					for (int _iter = 0; _iter < _count; _iter++) 
					{
						GUILayout.BeginVertical(GUIContent.none, _subviewStyle);
						{
							// Show each element
							EditorGUILayout.PropertyField(_arrayProperty.GetArrayElementAtIndex(_iter));
							
							// Show buttons 
							GUILayout.BeginHorizontal();
							{
								GUILayout.FlexibleSpace();
								
								if (GUILayout.Button("+", _buttonStyle, GUILayout.MinWidth(40)))
								{
									_arrayProperty.InsertArrayElementAtIndex(_iter);
									break;
								}
								
								if (GUILayout.Button("-", _buttonStyle, GUILayout.MinWidth(40)))
								{
									_arrayProperty.DeleteArrayElementAtIndex(_iter);
									break;
								}
							}
							GUILayout.EndHorizontal();
						}
						GUILayout.EndVertical();
					}
				}
			}

			// Reset indentation to original value
			EditorGUI.indentLevel	= _originalIndentLevel;
		}

		#endregion
	}
}
