using UnityEngine;
using System.Collections;

namespace VoxelBusters.Utility.UnityGUI.MENU
{
	public class GUISubMenu : GUIMenuBase
	{
		#region Properties

		private GameObject		m_gameObject;
		public GameObject		CachedGameObject
		{
			get 
			{ 
				if (m_gameObject == null)
					m_gameObject	= gameObject;

				return m_gameObject; 
			}
		}

		public string			SubMenuName
		{
			get { return CachedGameObject.name; }
		}

		#endregion

		#region Overriden Methods

		protected override void OnGUI ()
		{
			if (DrawTitleWithBackButton(SubMenuName, "<"))
			    OnPressingBackButton();
		}

		#endregion

		#region State

		public void SetActive (bool _newState)
		{
			CachedGameObject.SetActive(_newState);
		}

		public bool IsActive ()
		{
			return CachedGameObject.activeSelf;
		}

		#endregion

		#region GUI Events

		private void OnPressingBackButton ()
		{
			gameObject.SetActive(false);
		}

		#endregion
	}
}