using UnityEngine;
using System.Collections;

namespace VoxelBusters.Utility
{
	public class GUIScrollView : MonoBehaviour
	{
		#region Properties
		
		private Vector2 m_scrollPosition 	= Vector2.zero;
		private float	m_scrollSpeed	 	= 5.0f;
		private Rect 	m_rect 				= new Rect(0f,0f,0f,0f);
		
		#endregion
		
		#region Instance Methods
		
		public void BeginScrollView(GUIStyle _style, params GUILayoutOption[] _options)
		{
			m_scrollPosition = GUILayout.BeginScrollView(m_scrollPosition, _style, _options);
		}
		
		public void BeginScrollView(params GUILayoutOption[] _options)
		{
			BeginScrollView(GUI.skin.scrollView, _options);
		}
		
		public void EndScrollView()
		{
			GUILayout.EndScrollView();
			if (Event.current.type == EventType.Repaint)
			{
				m_rect = GUILayoutUtility.GetLastRect();
			}
		}
		
		public void Reset()
		{
			m_scrollPosition = Vector2.zero;	
		}
		
		#endregion
		
		
		#region Unity Methods
		
		void Update()
		{
			UpdateScroll();
		}
		
		#endregion
		
		#region Helpers
		
		void UpdateScroll()
		{
			
			//For touch based inputs
			foreach(Touch _touch in Input.touches)
			{
				
				Vector2 _posToCheck = _touch.position;
				_posToCheck.y = Screen.height - _posToCheck.y;
				
				if(_touch.phase == TouchPhase.Moved && m_rect.Contains(_posToCheck))
				{
					m_scrollPosition += _touch.deltaPosition * m_scrollSpeed;
					break; //Considering only one finger movement
				}
			}
			
		}
		
		#endregion
	}
}