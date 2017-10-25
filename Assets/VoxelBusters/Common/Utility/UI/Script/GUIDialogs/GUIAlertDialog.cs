using UnityEngine;
using System.Collections;

#if UNITY_EDITOR
namespace VoxelBusters.Utility
{
	public class GUIAlertDialog : GUIDialog 
	{
		#region Delegates

		public delegate void GUIAlertDialogResult (string _buttonPressed, string _callerTag);
				
		#endregion

		#region Public Properties

		public string 		Tag
		{
			get;
			set;
		}

		//Used for callback
		public GUIAlertDialogResult Delegate
		{
			get;
			set;
		}
		
		#endregion

		#region Creation Methods
		
		public static GUIAlertDialog Create(string _title, string _message, string[] _buttonList, GUIAlertDialogResult _delegate)
		{
			GameObject _alertDialogGameObject 	= new GameObject("AlertDialog");
			GUIAlertDialog _instance 			= _alertDialogGameObject.AddComponent<GUIAlertDialog>();


			_instance.Title 		= _title;
			_instance.Message 		= _message;
			_instance.ButtonList 	= _buttonList;
			_instance.Delegate		= _delegate;

			return _instance;
		}

		#endregion



		#region GUI Methods

		protected override void  OnGUIWindow()
		{		
			GUILayout.BeginHorizontal(); 
				GUILayout.FlexibleSpace();//This for placing in center
				GUILayout.BeginVertical();

					GUILayout.FlexibleSpace();
					GUILayout.BeginVertical(UISkin != null ? UISkin.scrollView : GUIStyle.none);//Specify style for background of this vertical layout
						DrawTitle();
						DrawMessage();
						DrawButtons(OnButtonPressed);
					GUILayout.EndVertical();
					GUILayout.FlexibleSpace();

				GUILayout.EndVertical();
				GUILayout.FlexibleSpace();
			GUILayout.EndHorizontal();	
		}

		#endregion


		#region Callbacks
		
		void OnButtonPressed(string _button)
		{
			Delegate(_button, Tag);
		}	

		#endregion
	}
}
#endif
