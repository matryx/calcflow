using UnityEngine;
using System.Collections;

namespace VoxelBusters.Utility
{
	public class GUIExtensions 
	{
		#region TextArea

		public static string TextArea (string _text, Rect _normalisedBounds)
		{
			Rect _absoluteBounds	= GetScreenSpaceBounds(_normalisedBounds);

			GUILayout.BeginArea(_absoluteBounds);
			{
				if (_text != null)
				{
					_text	= GUILayout.TextArea(_text, GUILayout.Width(_absoluteBounds.width),
					                           GUILayout.Height(_absoluteBounds.height));
				}
			}
			GUILayout.EndArea();

			return _text;
		}

		#endregion

		#region Button Layout

		public static void Buttons (ArrayList _buttonsList, System.Action<string> _callbackOnPress,
		                            Rect _normalisedBounds)
		{
			if (_buttonsList == null)
				return;

			Rect _absoluteBounds	= GetScreenSpaceBounds(_normalisedBounds);

			// Calculating height
			float _buttonHeight		= Screen.height * 0.05f;
			float _yOffsetBwButtons	= Screen.height * 0.01f;
			float _heightPerButton	= _buttonHeight + _yOffsetBwButtons;

			// Calculating total rows and columns
			int _totalButtons			= _buttonsList.Count;
			int _maxButtonsPerColumn	= (int)(_absoluteBounds.height / _heightPerButton);
			int _totalColumns			= ((int)(_totalButtons / _maxButtonsPerColumn)) + (((_totalButtons % _maxButtonsPerColumn) == 0) ? 0 : 1);

			// Drawing GUI buttons
			GUILayoutOption[] _layoutOptions	= new GUILayoutOption[] { GUILayout.Height(_buttonHeight) };

			GUILayout.BeginArea(_absoluteBounds);
			GUILayout.BeginHorizontal();
			{
				for (int _bIter = 0; _bIter < _totalColumns; _bIter++)
				{
					DrawButtonsLayout(_buttonsList, 					_callbackOnPress,
					                  _bIter * _maxButtonsPerColumn, 	_maxButtonsPerColumn, 
					                  _layoutOptions);
				}
			}
			GUILayout.EndHorizontal();
			GUILayout.EndArea();
		}

		private static void DrawButtonsLayout (ArrayList _buttonsList, System.Action<string> _callbackOnPress,
		                                       int _startingIndex, int _buttonsPerColumn, params GUILayoutOption[] _layoutOptions)
		{
			int _totalButtons	= _buttonsList.Count;
			int _endingIndex	= _startingIndex + _buttonsPerColumn;

			GUILayout.BeginVertical();
			{
				for (int _iter = _startingIndex; _iter < _endingIndex && _iter < _totalButtons; _iter++)
				{
					string _buttonName	= _buttonsList[_iter] as string;

					// Flexispace
					GUILayout.FlexibleSpace();

					if (GUILayout.Button(_buttonName, _layoutOptions))
					{
						if (_callbackOnPress != null)
							_callbackOnPress(_buttonName);
					}

					// Flexispace
					GUILayout.FlexibleSpace();
				}
			}
			GUILayout.EndVertical();
		}

		#endregion

		#region Misc. Methods

		private static Rect GetScreenSpaceBounds (Rect _normalisedBounds)
		{
			Rect _absoluteBounds	= new Rect();
			_absoluteBounds.xMin	= _normalisedBounds.xMin * Screen.width;
			_absoluteBounds.xMax	= _normalisedBounds.xMax * Screen.width;
			_absoluteBounds.yMin	= _normalisedBounds.yMin * Screen.height;
			_absoluteBounds.yMax	= _normalisedBounds.yMax * Screen.height; 

			return _absoluteBounds;
		}


		#endregion
	}
}
