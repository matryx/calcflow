using UnityEngine;
using UnityEditor;
using System.Collections;

namespace VoxelBusters.Utility
{
	[CustomPropertyDrawer(typeof(EnumMaskFieldAttribute))]
	public class EnumMaskFieldDrawer : PropertyDrawer 
	{
		#region Properties
		
		private EnumMaskFieldAttribute EnumMaskFieldAttribute 
		{ 
			get 
			{ 
				return ((EnumMaskFieldAttribute)attribute); 
			} 
		}
		
		#endregion

		#region Drawer Methods

		public override void OnGUI (Rect _position, SerializedProperty _property, GUIContent _label)
		{
			EditorGUI.BeginProperty(_position, _label, _property);

			// Draw enum mask field
			if (EnumMaskFieldAttribute.IsEnum())
			{
				// Start checking if property was changed
				EditorGUI.BeginChangeCheck();

				System.Enum _enumValue	= EditorGUI.EnumMaskField(_position, _label, EnumMaskFieldAttribute.GetEnumValue(_property));

				// Finish checking, update property value
				if (EditorGUI.EndChangeCheck())
					_property.intValue	= System.Convert.ToInt32(_enumValue);
			}
			else
			{
				base.OnGUI(_position, _property, _label);
			}

			EditorGUI.EndProperty();
		}
		
		#endregion
	}
}
