using UnityEngine;
using System.Collections;
using VoxelBusters.Utility;

namespace VoxelBusters.AssetStoreProductUtility.Demo
{
	public class DemoMainMenu : DemoGUIWindow 
	{
		private DemoSubMenu[] m_subMenuList;
	
		private DemoSubMenu	m_currentSubMenu;
	
	
		#region Unity Life Cycle Methods
		// Use this for initialization
		protected override void Start () 
		{
			base.Start();
	
			//Get list of all DemoSubMenu under this Main Menu
			m_subMenuList		= this.GetComponentsInChildren<DemoSubMenu>(true);
	
			//Setting MainMenu skin by default if UISkin not set for submenus
			foreach(DemoGUIWindow _each in m_subMenuList)
			{
				if(UISkin != null)
				{
					if(_each.UISkin == null)
					{
						_each.UISkin = UISkin;
					}
				}
			}
		
			//Disable all children initially
			DisableAllSubMenus();
		}
	
		private void Update ()
		{
	
			if(m_currentSubMenu != null && !m_currentSubMenu.gameObject.activeSelf)
			{
				m_currentSubMenu = null;
			}
	
		}

		#endregion

		#region Helpers

		private void DisableAllSubMenus()
		{
			foreach(DemoSubMenu each in m_subMenuList)
			{
				each.gameObject.SetActive(false);
			}
		}
	
		private void EnableSubMenu(DemoSubMenu _enabledSubMenu)
		{
			DisableAllSubMenus();
	
			//Enable new feature window
			_enabledSubMenu.gameObject.SetActive(true);
			
			//Save the window instance
			m_currentSubMenu = _enabledSubMenu;
	
		}

		#endregion
		
		#region Drawing

		protected override void OnGUIWindow()
		{		
			if (m_currentSubMenu == null)
			{
				RootScrollView.BeginScrollView();
				{
					GUILayout.Box(name);
					
					//If on, just list all the features
					for(int _i = 0 ; _i <  m_subMenuList.Length ; _i++)
					{
						DemoSubMenu  _each = 	m_subMenuList[_i];
						if(GUILayout.Button(_each.gameObject.name))
						{
							EnableSubMenu(_each);
							break;
						}
					}
				}
				RootScrollView.EndScrollView();
				
				GUILayout.FlexibleSpace();
			}
		}

		#endregion
	}
}
