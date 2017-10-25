using UnityEngine;
using System.Collections;

namespace VoxelBusters.Utility.UnityGUI.MENU
{
	public class GUIMenuBase : MonoBehaviour 
	{ 
		#region Properties

		// Layout specification
		private Rect 		m_titleLayoutNormalisedRect 	= new Rect(0.05f, 	0f, 	0.9f, 	0.2f);
		private Rect 		m_buttonLayoutNormalisedRect 	= new Rect(0.05f, 	0.2f, 	0.9f, 	0.8f);
		private Rect 		m_resultLayoutNormalisedRect 	= new Rect(0f, 		0.8f, 	1f, 	1f);
		private float		m_buttonColumnCount				= 2;

		#endregion

		#region Constants
		
		// Related to buttons
		public const float 	kButtonHeight					= 40f;
		public const float 	kBackButtonWidth				= 40f;
		public const float 	kBackButtonHeight				= 40f;
		
		#endregion

		#region Unity Methods
		
		protected virtual void OnEnable ()
		{
		}
		
		protected virtual void OnDisable ()
		{
		}
		
		protected virtual void OnGUI ()
		{
		}
		
		#endregion

		#region Title Section

		public void DrawTitle (string _title)
		{
			DrawTitleWithBackButton(_title, null);
		}

		public bool DrawTitleWithBackButton (string _title, string _button)
		{
			bool _backButtonPressed	= false;

			// Back button
			if (!string.IsNullOrEmpty(_button))
			{
				// GUI style
				GUIStyle _backBtnStyle 	= new GUIStyle(GUI.skin.button);
				_backBtnStyle.alignment	= TextAnchor.MiddleCenter;

				if (GUI.Button(new Rect(10f, 5f, kBackButtonWidth, kBackButtonHeight), 
				               _button, _backBtnStyle))
					_backButtonPressed	= true;
			}

			// Area reserved for title section
			GUILayout.BeginArea(GetLayoutRect(m_titleLayoutNormalisedRect));
			{
				// GUI style
				GUIStyle _titleStyle	= new GUIStyle(GUI.skin.label);
				_titleStyle.alignment	= TextAnchor.UpperCenter;
				_titleStyle.fontSize	= 20;

				// Spacing
				GUILayout.Space(10f);

				// Title
				GUILayout.Label(_title, _titleStyle);
			}
			GUILayout.EndArea();

			return _backButtonPressed;
		}

		#endregion
			
		#region Button Section
			
		public void BeginButtonLayout (float _columnCount = 2, float _normalisedHeight = 0.8f)
		{
			// Update normalised height
			m_buttonLayoutNormalisedRect.height	= _normalisedHeight;

			// Update button column count
			m_buttonColumnCount					= _columnCount;

			GUILayout.BeginArea(GetLayoutRect(m_buttonLayoutNormalisedRect));
			GUILayout.BeginHorizontal();
		}

		public void EndButtonLayout ()
		{
			GUILayout.EndHorizontal();
			GUILayout.EndArea();
		}

		public bool DrawButton (string _buttonName)
		{
			if (GUILayout.Button(_buttonName, GUILayout.MinHeight(kButtonHeight), 
			                     GUILayout.MaxWidth(Screen.width * m_buttonLayoutNormalisedRect.xMax / m_buttonColumnCount)))
				return true;

			return false;
		}
		
		#endregion

		#region Result section

		public void DrawResultLayout (string _result, float _normalisedHeight = 0.2f)
		{
			// Update normalised height
			m_resultLayoutNormalisedRect.height	= _normalisedHeight;

			if (m_resultLayoutNormalisedRect.height == 0f)
				return;
			
			// GUI style
			GUIStyle _resultStyle 	= new GUIStyle(GUI.skin.textArea);

			// Label
			GUI.Label(GetLayoutRect(m_resultLayoutNormalisedRect), _result, _resultStyle);
		}

		#endregion	

		#region Misc.

		private Rect GetLayoutRect (Rect _normalisedRect)
		{
			Vector2 _dimension	= new Vector2(Screen.width, Screen.height);
			Rect _rect			= new Rect(_dimension.x * _normalisedRect.xMin, _dimension.y * _normalisedRect.yMin,
			                        _dimension.x * _normalisedRect.xMax, 		_dimension.y * _normalisedRect.yMax);

			return _rect;
		}

		#endregion
	}
}