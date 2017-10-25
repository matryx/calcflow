using UnityEditor;
using UnityEngine;
using System.Text.RegularExpressions;
// Source: http://blogs.unity3d.com/2012/09/07/property-drawers-in-unity-4/ 

namespace VoxelBusters.Utility
{
	[CustomPropertyDrawer(typeof(RegexAttribute))]
	public class RegexDrawer : PropertyDrawer 
	{
		#region Properties

		// These constants describe the height of the help box and the text field.
		const int kHelpHeight = 30;
		const int kTextHeight = 16;
		
		// Provide easy access to the RegexAttribute for reading information from it.
		RegexAttribute regexAttribute { get { return ((RegexAttribute)attribute); } }

		#endregion

		#region Drawer Methods

		// Here you must define the height of your property drawer. Called by Unity.
		public override float GetPropertyHeight (SerializedProperty prop,
		                                         GUIContent label) 
		{
			if (IsValid (prop))
				return base.GetPropertyHeight (prop, label);
			else
				return base.GetPropertyHeight (prop, label) + kHelpHeight;
		}
		
		// Here you can define the GUI for your property drawer. Called by Unity.
		public override void OnGUI (Rect position,
		                            SerializedProperty prop,
		                            GUIContent label) 
		{
			// Adjust height of the text field
			Rect textFieldPosition = position;
			textFieldPosition.height = kTextHeight;
			DrawTextField (textFieldPosition, prop, label);
			
			// Adjust the help box position to appear indented underneath the text field.
			Rect helpPosition = EditorGUI.IndentedRect (position);
			helpPosition.y += kTextHeight;
			helpPosition.height = kHelpHeight;
			DrawHelpBox (helpPosition, prop);
		}
		
		void DrawTextField (Rect position, SerializedProperty prop, GUIContent label) 
		{
			// Draw the text field control GUI.
			EditorGUI.BeginChangeCheck ();
			string value = EditorGUI.TextField (position, label, prop.stringValue);
			if (EditorGUI.EndChangeCheck ())
				prop.stringValue = value;
		}
		
		void DrawHelpBox (Rect position, SerializedProperty prop) 
		{
			// No need for a help box if the pattern is valid.
			if (IsValid (prop))
				return;
			
			EditorGUI.HelpBox (position, regexAttribute.helpMessage, MessageType.Error);
		}

		// Test if the propertys string value matches the regex pattern.
		bool IsValid (SerializedProperty prop) 
		{
			if (string.IsNullOrEmpty(prop.stringValue))
				return true;

			return Regex.IsMatch (prop.stringValue, regexAttribute.pattern);
		}

		#endregion
	}
}