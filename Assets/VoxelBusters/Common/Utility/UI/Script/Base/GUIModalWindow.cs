using UnityEngine;
using System.Collections;

namespace VoxelBusters.Utility
{
	public class GUIModalWindow : MonoBehaviour
	{	
		#region Properties
		[SerializeField]
		private GUISkin m_uiSkin;
		
		//Used for restoring state of skin
		private GUISkin m_oldSkin;
		
		private GUIScrollView m_rootScrollView ;

		protected GUIScrollView RootScrollView 
		{
			get 
			{
				return m_rootScrollView;
			}
		}
		
		public GUISkin UISkin
		{
			get
			{
				return m_uiSkin;
			}
			set
			{
				if (value != null)
					m_uiSkin	= Instantiate(value) as GUISkin;
			}
		}

		protected Rect 	m_windowRect = new Rect(0f, 0f, Screen.width, Screen.height);
		
		#endregion

		#region Unity Methods

		protected virtual void Awake ()
		{
			UISkin = m_uiSkin;
		}
		
		protected virtual void Start ()
		{	
			//For drawing results in scrollview
			m_rootScrollView  =  gameObject.AddComponent<GUIScrollView>();
		}

		protected virtual void OnEnable ()
		{}
		
		protected virtual void OnDisable ()
		{}
		
		#endregion
		
		#region Drawing

		private void OnGUI()
		{
			SetSkin();
			
			//For resizing based on screen size
			AdjustFontBasedOnScreen();
			AdjustWindowBasedOnScreen();
			
			m_windowRect = GUI.Window(this.GetInstanceID(), m_windowRect, GUIWindowBase, "");
			
			RestoreSkin();	
		}
		
		private void GUIWindowBase(int _windowID)
		{
			OnGUIWindow();
		}
		
		protected virtual void OnGUIWindow()
		{
			//Draw by overriding this method
		}

		#endregion

		#region Helpers

		protected virtual void AdjustFontBasedOnScreen()
		{
			GUI.skin.box.fontSize 						= (int)(Screen.width * 0.03f);
			GUI.skin.button.fontSize 					= (int)(Screen.width * 0.03f);
			GUI.skin.label.fontSize 					= (int)(Screen.width * 0.03f);
			GUI.skin.toggle.fontSize 					= (int)(Screen.width * 0.03f);
		}
		
		protected virtual void AdjustWindowBasedOnScreen()
		{
			m_windowRect.width  	= Screen.width;
			m_windowRect.height 	= Screen.height;
		}
	
		protected void SetSkin()
		{
			m_oldSkin 	= GUI.skin;
			GUI.skin	= UISkin;
		}
		
		protected void RestoreSkin()
		{
			GUI.skin  = m_oldSkin;
		}
		
		protected float GetWindowWidth()
		{
			return m_windowRect.width;
		}
		
		protected float GetWindowHeight()
		{
			return m_windowRect.height;
		}
		
		#endregion
	}
}