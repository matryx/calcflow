using UnityEngine;
using System.Collections;

namespace VoxelBusters.Utility.UnityGUI.MENU
{
	public class GUIMainMenu : GUIMenuBase 
	{
		#region Properties

		private GUISubMenu[]					m_subMenuList;
		private GUISubMenu						m_activeSubMenu;

		// Controls whether main menu needs to be shown or sub menu
		private bool 							m_showingMainMenu;

		#endregion

		#region Constants

		private const int 						kButtonsPerColumn		= 5;

		#endregion

		#region Unity Methods

		protected virtual void Start ()
		{
			// Get all submenu's
			m_subMenuList		= GetComponentsInChildren<GUISubMenu>();

			// Show main menu by default
			m_showingMainMenu	= true;

			// Disable sub menu screens
			for (int _iter = 0; _iter < m_subMenuList.Length; _iter++)
				m_subMenuList[_iter].SetActive(false);
		}

		private void Update ()
		{
			if (!m_showingMainMenu)
				m_showingMainMenu	= !m_activeSubMenu.IsActive();
		}

		protected override void OnGUI ()
		{
			if (m_showingMainMenu)
				DrawMainMenu();
		}

		protected virtual void DrawMainMenu ()
		{
			// Title
			DrawTitle("Main Menu");

			// Sub menu is broken into two columns
			int _totalSubMenuCount	= m_subMenuList.Length;
			int _columnCount		= (_totalSubMenuCount / kButtonsPerColumn) + ((_totalSubMenuCount % kButtonsPerColumn) == 0 ? 0 : 1);

			BeginButtonLayout(_columnCount, 0.8f);
			{				
				DrawSubMenuColumn(m_subMenuList, 0, kButtonsPerColumn);
				DrawSubMenuColumn(m_subMenuList, kButtonsPerColumn, _totalSubMenuCount);
			}
			EndButtonLayout();
		}

		private void DrawSubMenuColumn (GUISubMenu[] _subMenuList, int _startIndex, int _endIndex)
		{
			int _subMenuCount	= _subMenuList.Length;

			GUILayout.BeginVertical();
			for (int _iter = _startIndex; (_iter < _endIndex && _iter < _subMenuCount); _iter++)
			{
				GUISubMenu _subMenu	= _subMenuList[_iter];
				
				// On clicking this button, enable appropriate menu
				if (DrawButton(_subMenu.SubMenuName))
				{
					// Enable submenu
					m_activeSubMenu		= _subMenu;
					m_activeSubMenu.SetActive(true);

					// Not showing main menu any more
					m_showingMainMenu	= false;
				}
			}
			GUILayout.EndVertical();
		}

		#endregion
	}
}