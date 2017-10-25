using UnityEngine;
using UnityEditor;
using System.Collections;

namespace VoxelBusters.Utility
{
	public class ArrayDrawer 
	{
		#region Properties

		public enum eArrayOptions
		{
			DEFAULT					= 0,
			SHOW_NAME_WITH_FOLDOUT	= 0x10,
			SHOW_ARRAY_SIZE			= 0x100,
		}

		private const float	kSpacingPixels		= 5f;
		private const float kEditButtonWidth	= 30f;

		#endregion
	
		#region Drawer Methods

		public static float GetPropertyHeight (SerializedProperty _arrayProperty, eArrayOptions _options)
		{
			int _count				= _arrayProperty.arraySize;
			bool _showArraySize		= (_options & eArrayOptions.SHOW_ARRAY_SIZE) != 0;
			float _singleLineHeight	= EditorGUIUtility.singleLineHeight;
			float _totalHeight		= 0f;

			// Height for array name
			_totalHeight		+= _singleLineHeight + kSpacingPixels;

			// Is foldout enabled, then dont show the rest of the elements
			if (_arrayProperty.isExpanded)
			{
				// If we showing array size then we need to consider it
				if (_showArraySize && _count != 0)
					_totalHeight	+= (EditorGUIUtility.singleLineHeight + kSpacingPixels);

				// If there are no elements then we will show button to add elements
				if (_count == 0)
				{
					// Add height of button and extra spacing
					_totalHeight	+= (EditorGUIUtility.singleLineHeight + kSpacingPixels);
				}
				// We do have contents within array
				else
				{
					// Includes height for each element, height of edit bar, spacing
					for (int _iter = 0; _iter < _count; _iter++)
						_totalHeight+= EditorGUI.GetPropertyHeight(_arrayProperty.GetArrayElementAtIndex(_iter)) + EditorGUIUtility.singleLineHeight + kSpacingPixels;
				}
			}
			
			return _totalHeight;
		}

		public static void Draw (Rect _position, SerializedProperty _arrayProperty, GUIContent _label, eArrayOptions _options)
		{
			int _originalIndentLevel	= EditorGUI.indentLevel;
			int _count					= _arrayProperty.arraySize;
			bool _showNameWithFoldout	= (_options & eArrayOptions.SHOW_NAME_WITH_FOLDOUT) != 0;
			bool _showArraySize			= (_options & eArrayOptions.SHOW_ARRAY_SIZE) != 0;

			// Height used for primitive properties and buttons
			float _singleLineHeight		= EditorGUIUtility.singleLineHeight;
			
			// Calculate rect
			float _positionY			= _position.y;

			// Rect for array name
			Rect _nameRect				= new Rect(_position.x, _positionY, _position.width, _singleLineHeight);
			_positionY					+= (_singleLineHeight + kSpacingPixels);
			
			// Show array name
			if (_showNameWithFoldout)
			{
				_arrayProperty.isExpanded	= EditorGUI.Foldout(_nameRect, _arrayProperty.isExpanded, _label);

				// Indent to next level
				EditorGUI.indentLevel++;
			}
			else
			{
				EditorGUI.LabelField(_nameRect, _label);
			}

			// Is foldout enabled, then dont show the rest of the elements
			if (!_arrayProperty.isExpanded)
			{
				// Reset indentation level
				EditorGUI.indentLevel	= _originalIndentLevel;
				return;
			}

			// Show array size
			if (_showArraySize && _count != 0)
			{
				// Rect for array Length
				Rect _sizeRect		= new Rect(_position.x, _positionY, _position.width, _singleLineHeight);
				_positionY			+= (_singleLineHeight + kSpacingPixels);

				// Check if size value changes
				EditorGUI.BeginChangeCheck();
				int _newSize		= EditorGUI.IntField(_sizeRect, "Size", _count);

				if (EditorGUI.EndChangeCheck())
					_arrayProperty.arraySize	= _newSize;
			}			

			// If there are no elements then we will show button to add elements
			if (_count == 0)
			{
				// Rect for add button
				Rect _addButtonRect	= new Rect(_position.x, _positionY, _position.width, _singleLineHeight);
				_positionY			+= (_singleLineHeight + kSpacingPixels);

				if (GUI.Button(_addButtonRect, "Add"))
					_arrayProperty.InsertArrayElementAtIndex(0);
			}
			else
			{
				// If there are elements then we will show its contents
				for (int _iter = 0; _iter < _count; _iter++)
				{
					// Get element property
					SerializedProperty _elementProperty	= _arrayProperty.GetArrayElementAtIndex(_iter);
					float _elementHeight				= EditorGUI.GetPropertyHeight(_elementProperty);

					// Rect for element, edit buttons
					Rect _elementRect		= new Rect(_position.x, _positionY, _position.width, _elementHeight);
					_positionY				+= _elementHeight;
					Rect _deleteButtonRect	= new Rect(_position.x + _position.width - kEditButtonWidth, _positionY, 
					                                  kEditButtonWidth, _singleLineHeight);
					Rect _addButtonRect		= new Rect(_deleteButtonRect.x - kEditButtonWidth, _positionY, 
					                                kEditButtonWidth, _singleLineHeight);
					_positionY				+= _singleLineHeight + kSpacingPixels;

					// Grouping element and buttons
					EditorGUI.PropertyField(_elementRect,
					                        _elementProperty, 
					                        new GUIContent("# " + (_iter + 1).ToString() + ":"), 
					                        true);
					
					if (GUI.Button(_addButtonRect, "+"))
					{
						_arrayProperty.InsertArrayElementAtIndex(0);
						break;
					}
					
					if (GUI.Button(_deleteButtonRect, "-"))
					{
						_arrayProperty.DeleteArrayElementAtIndex(_iter);
						break;
					}
				}
			}

			// Reset indentation level
			EditorGUI.indentLevel	= _originalIndentLevel;
		}
		
		#endregion
	}
}