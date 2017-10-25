using UnityEngine;
using System.Collections;

namespace VoxelBusters.Utility
{
	public class UnityGUILayoutUtility : MonoBehaviour 
	{
		public static bool Foldout (GUIContent _label, bool _state)
		{
			// Enable rich text
			GUIStyle _style			= new GUIStyle("label");
			_style.richText			= true;
			
			// Create new label
			GUIContent _labelCopy	= new GUIContent(_label);
			string _toggleSymbol	= null;
			
			if (_state) 
				_toggleSymbol	= "-";
			else 
				_toggleSymbol 	= "+";
			
			// Append tags
			_labelCopy.text		= string.Format("<b>{0} {1} </b>", _toggleSymbol, _label.text);
			
			GUILayout.BeginHorizontal();
			{
				if (!GUILayout.Toggle(true, _labelCopy, _style)) 
					_state = !_state;
			}
			GUILayout.EndHorizontal();
			
			return _state;
		}
	}
}
