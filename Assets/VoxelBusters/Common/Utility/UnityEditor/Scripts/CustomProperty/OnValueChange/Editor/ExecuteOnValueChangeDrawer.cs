using UnityEngine;
using UnityEditor;
using System.Collections;
using System.Reflection;

namespace VoxelBusters.Utility
{
	[CustomPropertyDrawer(typeof(ExecuteOnValueChangeAttribute), true)]
	public class ExecuteOnValueChangeDrawer : PropertyDrawer 
	{
		#region Properties

		private ExecuteOnValueChangeAttribute ExecuteOnValueChange 
		{ 
			get 
			{ 
				return (ExecuteOnValueChangeAttribute)attribute; 
			} 
		}

		#endregion

		#region Drawer Methods

		public override float GetPropertyHeight (SerializedProperty _property, GUIContent _label) 
		{
			return EditorGUI.GetPropertyHeight (_property);
		}

		public override void OnGUI (Rect _position, SerializedProperty _property, GUIContent _label)
		{
			EditorGUI.BeginProperty (_position, _label, _property);

			// Start checking if property was changed
			EditorGUI.BeginChangeCheck();
			{
				EditorGUI.PropertyField (_position, _property, _label, true);
			}
			bool _valueChanged	= EditorGUI.EndChangeCheck ();

			// If value has changed then invoke the method
			if (_valueChanged)
			{
				SerializedObject	_serializedObject	= _property.serializedObject;

				_serializedObject.ApplyModifiedProperties ();
				_serializedObject.targetObject.InvokeMethod (ExecuteOnValueChange.InvokeMethod);
			}

			EditorGUI.EndProperty ();
		}

		#endregion
	}
}