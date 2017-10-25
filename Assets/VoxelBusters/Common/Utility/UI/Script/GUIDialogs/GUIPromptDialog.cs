using UnityEngine;
using System.Collections;

#if UNITY_EDITOR
namespace VoxelBusters.Utility
{
	public class GUIPromptDialog : GUIDialog 
	{
	
		#region Inner Classes

		public class InputFieldElement
		{
			public string 	PlaceholderText;
			public bool		IsSecure;
			public bool		IsEnabled;

			private	string	CurrentText = "";//Used for internal purposes
			
			public void Set(string _placeholderText = "", bool _isSecure = false, bool _isEnabled = true, string _defaultText = "")
			{
				PlaceholderText = _placeholderText;
				IsSecure = _isSecure;
				IsEnabled = _isEnabled;

				SetCurrentText(_defaultText);
			}

			public void SetCurrentText(string _text)
			{
				CurrentText = _text;
			}

			public string GetCurrentText()
			{
				return CurrentText;
			}
		}

		#endregion

		#region Delegates

		public delegate void GUIPromptDialogResult (string _buttonPressed, InputFieldElement[] _inputList);

		#endregion


		#region Public Properties

		//Used for callback
		public InputFieldElement[] InputList
		{
			get;
			set;
		}

		//Used for callback
		public GUIPromptDialogResult Delegate
		{
			get;
			set;
		}
		
		#endregion

		#region Creation Methods
		
		//string _placeholder, bool _useSecureText
		public static GUIPromptDialog Create(string _title, string _message, InputFieldElement[] _inputList, string[] _buttonList, GUIPromptDialogResult _delegate)
		{
			GameObject _dialogGameObject 		= new GameObject("PromptDialog");
			GUIPromptDialog _instance 			= _dialogGameObject.AddComponent<GUIPromptDialog>();
			
			
			_instance.Title 		= _title;
			_instance.Message 		= _message;
			_instance.InputList 	= _inputList;
			_instance.ButtonList 	= _buttonList;

			_instance.Delegate		= _delegate;
			
			return _instance;
		}

		#endregion	


		#region GUI Methods
		
		protected override void  OnGUIWindow()
		{		
			
			GUILayout.BeginHorizontal(); //This for placing in center
				GUILayout.FlexibleSpace();
				GUILayout.BeginVertical();
				
					GUILayout.FlexibleSpace();
					GUILayout.BeginVertical(UISkin != null ? UISkin.scrollView : GUIStyle.none);//Specify style for background of this vertical layout
						DrawTitle();
						DrawMessage();
						DrawInputFields();
						DrawButtons(OnButtonPressed);
					GUILayout.EndVertical();
					GUILayout.FlexibleSpace();
					
				GUILayout.EndVertical();
				GUILayout.FlexibleSpace();
			GUILayout.EndHorizontal();	
			
		}

		void DrawInputFields()
		{
			if(InputList != null)
			{
				foreach(InputFieldElement _each in InputList)
				{
					_each.SetCurrentText(!_each.IsSecure ? GUILayout.TextField(_each.GetCurrentText()) : GUILayout.PasswordField(_each.GetCurrentText(),'*'));
				}
			}
		}
		
		#endregion


		#region Callbacks
		
		void OnButtonPressed(string _button)
		{
			if(Delegate != null)
			{
				InputFieldElement[] _inputList = (InputFieldElement[])InputList.Clone();//Create a new copy and send as this object will be destroyed
				Delegate(_button, _inputList);
			}
		}	
	
		#endregion
	}

}
#endif
