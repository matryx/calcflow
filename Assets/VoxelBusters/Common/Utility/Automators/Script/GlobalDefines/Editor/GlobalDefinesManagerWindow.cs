//The MIT License (MIT)
//	
//Copyright (c) 2013 Karl Henkel
//
//Permission is hereby granted, free of charge, to any person obtaining a copy of
//this software and associated documentation files (the "Software"), to deal in
//the Software without restriction, including without limitation the rights to
//use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
//the Software, and to permit persons to whom the Software is furnished to do so,
//subject to the following conditions:
//
//The above copyright notice and this permission notice shall be included in all
//copies or substantial portions of the Software.
//
//THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
//FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
//COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
//IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
//CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#if UNITY_EDITOR && !(UNITY_WEBPLAYER || UNITY_WEBGL)
using UnityEngine;
using UnityEditor;
using System.Collections;
using System.Collections.Generic;

namespace VoxelBusters.Utility
{
	public class GlobalDefinesManagerWindow : EditorWindow
	{
		#region Properties

		private				GlobalDefinesManager 				m_definesManager;
		private				GlobalDefinesManager.eCompiler 		m_compiler;

		// Related to UI
		private				Vector2								m_scrollPostion		= Vector2.zero;

		#endregion

		#region Constants

		private		const	int									kCompilerCount		= 4;

		#endregion

		#region Menu Methods

		[MenuItem("Window/Global Defines")]
		public static void OpenDefinesManager ()
		{
			EditorWindow.GetWindow<GlobalDefinesManagerWindow>(true, "Global Defines Manager", true);
		}

		#endregion

		#region Methods
		
		private void OnEnable ()
		{
			m_definesManager	= new GlobalDefinesManager();
			m_compiler			= GlobalDefinesManager.eCompiler.CSHARP;
		}
		
		private void OnGUI ()
		{
			Color 	_oldColor 		= GUI.backgroundColor;

			// ******************
			// Menu Display
			// ******************
			GUILayout.BeginHorizontal();
			{
				for (int _iter = 0; _iter < kCompilerCount; _iter++)	
				{
					if (_iter == (int)m_compiler)
						GUI.backgroundColor = Color.gray;
					
					GUIStyle _style;

					switch(_iter)
					{
					case 0:
						_style = EditorStyles.miniButtonLeft;
						break;
					case (kCompilerCount - 1):
						_style = EditorStyles.miniButtonRight;
						break;
					default:
						_style = EditorStyles.miniButtonMid;
						break;
					}

					GlobalDefinesManager.eCompiler	_compiler	= (GlobalDefinesManager.eCompiler)_iter;

					if (GUILayout.Button(_compiler.ToString(), _style))
					{
						m_compiler	= _compiler;
					}
					
					GUI.backgroundColor = _oldColor;
				}
			}
			GUILayout.EndHorizontal();

			// ******************
			// Content Display
			// ******************
			GUILayout.Label(m_compiler.ToString() + " User Defines");
			
			m_scrollPostion	 				= GUILayout.BeginScrollView(m_scrollPostion);
			{
				List<string>	_definesList	= m_definesManager.GetDefinesList(m_compiler);

				for (int _iter = 0; _iter < _definesList.Count; _iter++)
				{
					GUILayout.BeginHorizontal();
					{
						_definesList[_iter] = EditorGUILayout.TextField(_definesList[_iter]);
						
						GUI.backgroundColor = Color.red;
						if (GUILayout.Button("x", GUIStyle.none, GUILayout.MaxWidth(18)))
							_definesList.RemoveAt(_iter);
						
						GUI.backgroundColor = _oldColor;
					}
					GUILayout.EndHorizontal();
				}
				
				GUILayout.Space(4);
				
				GUI.backgroundColor 	= Color.cyan;

				if (GUILayout.Button("Add"))	
					m_definesManager.AddNewDefineSymbol(m_compiler, "NEW_DEFINE");
			}
			GUILayout.EndScrollView();
			
			GUILayout.BeginHorizontal();
			{
				GUI.backgroundColor = Color.green;

				if (GUILayout.Button("Apply"))
				{
					m_definesManager.SaveCompiler(m_compiler);
				}
			
				GUI.backgroundColor = Color.red;
			
				if (GUILayout.Button("Apply All", GUILayout.MaxWidth(64)))
				{
					m_definesManager.SetAllCompilerDefines(m_compiler);
					m_definesManager.SaveAllCompilers();
				}
			}
			GUILayout.EndHorizontal();

			GUI.backgroundColor = _oldColor;
		}

		#endregion
	}
}
#endif