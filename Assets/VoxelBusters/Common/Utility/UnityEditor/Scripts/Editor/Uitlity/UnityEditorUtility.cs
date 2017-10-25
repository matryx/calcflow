using UnityEngine;
using UnityEditor;
using System.Collections;
using System.Text.RegularExpressions;

namespace VoxelBusters.Utility
{
	public class UnityEditorUtility : Editor 
	{
		public enum Alignment
		{
			None,
			Left,
			Center,
			Right
		}

		#region Constants 

		private const int 				kTabSize				= 12;

		// GUI styles
		public const string				kOuterContainerStyle	= "Tooltip";
		public const string				kContainerStyle			= "HelpBox";
	
		#endregion

		#region Static Fields
		
		private		static		GUIStyle		splitterStyle;
		
		#endregion
		
		#region Constructor
		
		static UnityEditorUtility ()
		{
			// Create splitter style
			splitterStyle					= new GUIStyle();
			splitterStyle.normal.background	= Texture2D.whiteTexture;
			splitterStyle.stretchWidth		= true;
		}
		
		#endregion

		#region Splitter Methods
		
		public static void DrawSplitter (Color _color, float _thickness = 1f, int _margin = 0)
		{
			splitterStyle.margin	= new RectOffset(_margin, _margin, 7, 7);
			Rect	_rectPosition	= GUILayoutUtility.GetRect(GUIContent.none, splitterStyle, GUILayout.Height(_thickness));
			
			if (Event.current.type == EventType.repaint)
			{
				Color 	_resetColor = GUI.color;
				GUI.color 			= _color;
				
				// Draw
				splitterStyle.Draw(_rectPosition, false, false, false, false);
				
				// Reset values
				GUI.color	 		= _resetColor;
			}
		}
		
		#endregion

		#region Label Methods
		
		public static void DrawLabel (string _text, Alignment _alignment = Alignment.None)
		{
			DrawLabel(_text, GUI.skin.label, _alignment);
		}
		
		public static void DrawLabel (string _text, GUIStyle _style, Alignment _alignment = Alignment.None)
		{
			GUILayout.BeginHorizontal();
			{
				switch (_alignment)
				{
				case Alignment.None:
					GUILayout.Label(_text, _style);
					break;
					
				case Alignment.Left:
					GUILayout.Label(_text, _style);
					GUILayout.FlexibleSpace();
					break;
					
				case Alignment.Center:
					GUILayout.FlexibleSpace();
					GUILayout.Label(_text, _style);
					GUILayout.FlexibleSpace();
					break;
					
				case Alignment.Right:
					GUILayout.FlexibleSpace();
					GUILayout.Label(_text, _style);
					break;
				}
			}
			GUILayout.EndHorizontal();
		}
		
		#endregion
		
		#region Button Method
		
		public static bool DrawButton (Rect _position, string _text, bool _enabled)
		{
			return DrawButton(_position, _text, GUI.skin.button, _enabled);
		}
		
		public static bool DrawButton (Rect _position, string _text, GUIStyle _style, bool _enabled = true)
		{
			bool	_resetState		= GUI.enabled;
			GUI.enabled				= _enabled;
			bool	_buttonPressed	= GUI.Button(_position, _text, _style);
			
			// Reset GUI state
			GUI.enabled				= _resetState;
			
			return _buttonPressed;
		}
		
		public static bool DrawButton (string _text, Alignment _alignment = Alignment.None, bool _enabled = true, params GUILayoutOption[] _options)
		{
			return DrawButton(_text, GUI.skin.button, _alignment, _enabled, _options);
		}
		
		public static bool DrawButton (string _text, GUIStyle _style, Alignment _alignment = Alignment.None, bool _enabled = true, params GUILayoutOption[] _options)
		{
			bool 	_isPressed		= false;
			bool	_resetState		= GUI.enabled;
			GUI.enabled				= _enabled;
			
			GUILayout.BeginHorizontal();
			{
				switch (_alignment)
				{
				case Alignment.None:
					_isPressed = GUILayout.Button(_text, _style, _options);
					break;
					
				case Alignment.Left:
					_isPressed = GUILayout.Button(_text, _style, _options);
					GUILayout.FlexibleSpace();
					break;
					
				case Alignment.Center:
					GUILayout.FlexibleSpace();
					_isPressed = GUILayout.Button(_text, _style, _options);
					GUILayout.FlexibleSpace();
					break;
					
				case Alignment.Right:
					GUILayout.FlexibleSpace();					
					_isPressed = GUILayout.Button(_text, _style, _options);
					break;
				}
			}
			GUILayout.EndHorizontal();
			
			// Reset GUI state
			GUI.enabled				= _resetState;
			
			return _isPressed;
		}
		
		#endregion

		#region Header Methods

		public static bool DrawPropertyHeader (SerializedProperty _property)
		{
			bool		_isExpandedOld	= _property.isExpanded;
			string		_displayName	= _property.displayName;
			string 		_toolTip		= _property.tooltip;

			// Draw toggle
			_property.isExpanded		= DrawPropertyHeader(_isExpandedOld, _displayName, _toolTip);
			
			return (_property.isExpanded != _isExpandedOld);
		}

		public static bool DrawPropertyHeader (bool _status, GUIContent _text)
		{
			return DrawPropertyHeader(_status, _text.text, _text.tooltip);
		}

		public static bool DrawPropertyHeader (bool _status, string _text, string _toolTip)
		{
			GUIContent	_newText		= new GUIContent(_text, _toolTip);
			GUIStyle 	_toggleStyle	= new GUIStyle("LargeButton");
			_toggleStyle.richText		= true;
			
			return GUILayout.Toggle(_status, _newText, _toggleStyle);
		}
		
		#endregion
	}
}