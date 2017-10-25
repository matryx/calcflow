using UnityEngine;
using System.Collections;

#if UNITY_EDITOR
using UnityEditor;

namespace VoxelBusters.Utility
{
	public class PromptWindow : EditorWindow 
	{
		#region Properties

		public 		string						Message
		{
			get;
			set;
		}

		public 		string						Description
		{
			get;
			set;
		}

		public 		string[]					Buttons
		{
			get;
			set;
		}

		public 		System.Action<string>		CallbackOnDismiss
		{
			get;
			set;
		}

		protected 	Vector2						ScrollPosition
		{
			get;
			set;
		}

		protected 	Vector2						ScrollSize
		{
			get;
			set;
		}

		protected 	Vector2						WindowSize
		{
			get;
			private set;
		}

		protected	 string						ButtonPressedOnClose
		{
			get;
			private set;
		}

		#endregion

		#region Unity Methods

		protected virtual void OnEnable ()
		{
			// Set size of window
			WindowSize	= new Vector2(480f, 320f);
			minSize		= WindowSize;
			maxSize		= WindowSize;
			ScrollSize	= WindowSize;
		}

		protected virtual void OnDestroy ()
		{
			if (CallbackOnDismiss != null)
				CallbackOnDismiss(ButtonPressedOnClose);
		}

		protected virtual void OnDisable ()
		{}
		
		protected virtual void OnGUI () 
		{
			// GUI style
			GUIStyle _messageGUIStyle		= new GUIStyle("LargeLabel");
			_messageGUIStyle.wordWrap		= true;

			GUIStyle _descriptionGUIStyle	= new GUIStyle("WordWrappedLabel");
			
			// Message section
			GUILayout.Label(Message, _messageGUIStyle);
			GUILayout.Space(10f);
			
			// Description section
			float _descriptionSectionHeight	= 220f;
			
			if (!string.IsNullOrEmpty(Description))
			{
				GUILayoutOption _svWidth	= GUILayout.Width(ScrollSize.x);
				GUILayoutOption _svHeight	= GUILayout.Height(_descriptionSectionHeight);
				
				ScrollPosition				= GUILayout.BeginScrollView(ScrollPosition, _svWidth, _svHeight);
				{
					GUILayout.Label(Description, _descriptionGUIStyle, GUILayout.Width(ScrollSize.x - 10f));
				}
				GUILayout.EndScrollView();
			}
			// Utilizing description space
			else
			{
				GUILayout.Space(_descriptionSectionHeight);
			}
			
			GUILayout.Space(20f);
			
			// Buttons section
			if (Buttons == null)
				return;
			
			bool _closeWindow	= false;
			
			// Buttons are right aligned
			GUILayout.BeginHorizontal();
			{
				GUILayout.FlexibleSpace();
				
				for (int _iter = 0; _iter < Buttons.Length; _iter++)
				{
					string _buttonName	= Buttons[_iter];
					
					if (GUILayout.Button(_buttonName, "LargeButton"))
					{
						// Close this window
						ButtonPressedOnClose	= _buttonName;
						_closeWindow			= true;
						break;
					}
				}
			}
			GUILayout.EndHorizontal();
			
			// Marked to close window
			if (_closeWindow)
				Close();
		}

		#endregion
	}
}
#endif