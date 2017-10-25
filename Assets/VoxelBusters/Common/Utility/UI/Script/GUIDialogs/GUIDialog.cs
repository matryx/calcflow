using UnityEngine;
using System.Collections;

#if UNITY_EDITOR
namespace VoxelBusters.Utility
{
	public class GUIDialog : GUIModalWindow 
	{
		#region Delegates
		
		protected delegate void GUIDialogResult (string _buttonPressed);
		
		#endregion
		
		#region Fields
		
		protected 	bool 	m_show;
		private		bool	m_returnPressed;
		
		#endregion

		#region Public Properties

		public string		Title
		{
			get;
			set;
		}
		
		public string 		Message 
		{
			get;
			set;
		}
		
		public string[] 	ButtonList
		{
			get;
			set;
		}

		#endregion

		protected override void Start()
		{
			base.Start();

			//Hide editor gui objects in hierarchy
			gameObject.hideFlags = HideFlags.HideInHierarchy;
		}

		private void Update ()
		{
			if (Input.GetKeyDown(KeyCode.Return))
				m_returnPressed	= true;
		}

		#region Visibility Control Methods
		
		public void Show()
		{
			m_show			= true;
			this.gameObject.SetActive(true);
		}
		
		public void Hide()
		{
			m_show			= false;
			this.gameObject.SetActive(false);
		}
		
		#endregion

		#region Overrides Drawing
		
		protected override void OnGUIWindow()
		{
			//Override to draw
		}
		
		#endregion

		#region Helpers

		protected void DrawTitle(float _widthFactor = 0.5f)
		{
			if(!string.IsNullOrEmpty(Title))
			{
				GUILayout.Box(Title, GUILayout.Width(Screen.width * _widthFactor));		
			}
		}

		protected void DrawMessage(float _widthFactor = 0.5f)
		{
			if(!string.IsNullOrEmpty(Message))
			{
				GUILayout.Label(Message, GUILayout.Width(Screen.width * _widthFactor));	
			}
		}

		protected void DrawButtons(GUIDialogResult _delegate, float _widthFactor = 0.5f)
		{
			if (ButtonList == null)
				return;

			GUILayoutOption _width			= GUILayout.Width(Screen.width * _widthFactor);
			bool			_buttonPressed	= false;

			if (ButtonList.Length <= 2)
			{
				GUILayout.BeginHorizontal(_width);
				{
					foreach (string each in ButtonList)
					{
						if (GUILayout.Button(each))
						{
							// Update flag
							_buttonPressed	= true;

							// Send the callback and destroy
							if (_delegate != null)
								_delegate(each);					

							// Close by destroying this object
							Close();
						}
					}
				}
				GUILayout.EndHorizontal();
			}
			else
			{
				GUILayout.BeginVertical(_width);
				{
					foreach (string each in ButtonList)
					{
						if (GUILayout.Button(each))
						{
							// Update flag
							_buttonPressed	= true;

							// Send the callback and destroy
							if (_delegate != null)
								_delegate(each);					

							// Close by destroying this object
							Close();
						}
					}
				}
				GUILayout.EndVertical();
			}

			// Check if enter was pressed
			if (!_buttonPressed && m_returnPressed)
			{
				// Send the callback and destroy
				if (_delegate != null)
					_delegate(ButtonList[0]);					
				
				// Close by destroying this object
				Close();
			}

			m_returnPressed	= false;
		}

		protected void Close()
		{
			Destroy(this.gameObject);
		}

		#endregion
	}
}
#endif
