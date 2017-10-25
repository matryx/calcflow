using UnityEngine;
using System.Collections;
using UnityEditor;

namespace VoxelBusters.Utility
{
	[CustomPropertyDrawer(typeof(InspectorButtonAttribute))]
	public class InspectorButtonDrawer : PropertyDrawer 
	{
		#region Properties
		
		private InspectorButtonAttribute InspectorButton
		{ 
			get 
			{ 
				return attribute as InspectorButtonAttribute; 
			} 
		}
		
		#endregion

		#region Constants

		private		const		float							kButtonWidth		= 228f;
		private		const		float							kButtonHeight		= 21f;
		private		const		float							kOffset				= 4f;

		#endregion

		#region Drawer Methods
		
		public override float GetPropertyHeight (SerializedProperty _property, GUIContent _label) 
		{
			if (_property.isArray)
				return EditorGUI.GetPropertyHeight(_property);

			return EditorGUI.GetPropertyHeight(_property) + kButtonHeight + kOffset;
		}

		public override void OnGUI (Rect _position, SerializedProperty _property, GUIContent _label)
		{
			EditorGUI.BeginProperty(_position, _label, _property);

			// Draw property
			if (_property.isArray)
			{
				EditorGUI.PropertyField(_position, _property, _label, true);
			}
			else
			{
				Rect 	_buttonRect;
				Rect	_propertyRect;
				float 	_basePropertyHeight		= EditorGUI.GetPropertyHeight(_property);

				if (InspectorButton.Position == InspectorButtonAttribute.ePosition.TOP)
				{
					_buttonRect					= new Rect((_position.xMin + _position.width - kButtonWidth) * 0.5f, _position.yMin, 
					                        				kButtonWidth, 		kButtonHeight);
					_propertyRect				= new Rect(_position.xMin, 		_position.yMin + kButtonHeight + kOffset, 
					                        				_position.width, 	_basePropertyHeight);
				}
				else
				{
					_propertyRect				= new Rect(_position.xMin, 		_position.yMin, 
					                        				_position.width, 	_basePropertyHeight);
					_buttonRect					= new Rect((_position.xMin + _position.width - kButtonWidth) * 0.5f, _position.yMin + _basePropertyHeight + kOffset, 
					                        				kButtonWidth, 		kButtonHeight);
				}

				// Draw property
				EditorGUI.PropertyField(_propertyRect, _property, _label, true);

				// Draw button
				if (GUI.Button(_buttonRect, InspectorButton.Name))
					_property.serializedObject.targetObject.InvokeMethod(InspectorButton.InvokeMethod);
			}

			EditorGUI.EndProperty();
		}
		
		#endregion
	}
}