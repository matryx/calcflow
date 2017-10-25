using UnityEngine;
using System.Collections;
using System.Collections.Generic;
using VoxelBusters.Utility;

#if UNITY_EDITOR
using UnityEditor;
#endif

namespace VoxelBusters.AssetStoreProductUtility
{
	using Internal;

	public class AssetStoreProduct
	{
		#region Properties

		public		string 					ProductName
		{	
			get;
			private set;
		}

		public		string					ProductVersion
		{
			get;
			private set;
		}

		public 	 	Texture2D				LogoTexture
		{
			get;
			private set;
		}

		private 	ProductUpdateInfo		m_productUpdateInfo;
		private 	bool					m_isAutomaticCheck;
		private 	GETRequest				m_updateGETRequest;
		#if UNITY_EDITOR
		private		UpdatePromptWindow		m_promptWindow;
		#endif

		#endregion

		#region Constants

		private const string 				kServerBaseAddress				= "https://unity3dplugindashboard.herokuapp.com";
		private const string				kProductDetailsPathFormat		= "products/{0}/details";

		// Related to update dialog
		private const string 				kCheckUpdatesFailedMessage		= "{0} update server could not be connected. Please try again after sometime.";
		private const string 				kAlreadyUptoDateMessage			= "You are using latest version of {0}. Please check back for updates at a later time.";
		private const string 				kNewVersionAvailableMessage		= "Newer version of {0} is available for download.";

		// Related to update request
		private const int					kCheckForUpdatesAfterMinutes	= 360;	
		private const string 				kSkippedVersionPrefix			= "version-skipped";

		#endregion

		#region Constructor

		private AssetStoreProduct ()
		{}

		public AssetStoreProduct (string _pName, string _pVersion, string _logoPath)
		{
			// Set properties
			ProductName					= _pName;
			ProductVersion				= _pVersion;

#if UNITY_EDITOR
			// Load logo texture
			LogoTexture					= AssetDatabase.LoadAssetAtPath(_logoPath, typeof(Texture2D)) as Texture2D;

			if (LogoTexture != null)
				LogoTexture.hideFlags	= HideFlags.HideAndDontSave;

			float _recheckAfterSecs		= kCheckForUpdatesAfterMinutes * 60f;
			float _firstCheckAfterSecs	= _recheckAfterSecs;

			// Check at unity application launch
			if (EditorApplication.timeSinceStartup < 100f)
			{
				_firstCheckAfterSecs	= 1f;
			}

			EditorInvoke.InvokeRepeating(AutoCheckForUpdates, _firstCheckAfterSecs, _recheckAfterSecs);
#endif
		}

		~AssetStoreProduct ()
		{
			LogoTexture	= null;
		}

		#endregion

		#region Product Updates Method

#if UNITY_EDITOR
		private string GetGlobalIdentificationForThisProduct ()
		{
			return ProductName.Replace(" ", "-").ToLower();
		}

		public void AutoCheckForUpdates ()
		{
			// If already a request is going on, then no need to auto check for updates
			if (m_updateGETRequest != null)
			{
				return;
			}

			// Check for updates
			CheckForUpdates(true);
		}
		
		public void CheckForUpdates (bool isAutoCheck = false) 
		{
			// Mark if request is auto or manual check
			m_isAutomaticCheck			= isAutoCheck;

			string _productName			= GetGlobalIdentificationForThisProduct();
			string _productDetailsPath	= string.Format(kProductDetailsPathFormat, _productName);
			URL _URL					= URL.URLWithString(kServerBaseAddress, _productDetailsPath);

			// Start asynchronous request
			GETRequest _request			= GETRequest.CreateAsyncRequest(_URL, null);
			_request.OnSuccess			= RequestForUpdatesSuccess;
			_request.OnFailure			= RequestForUpdatesFailed;

			// Start request
			_request.StartRequest();

			// Cache request
			m_updateGETRequest			= _request;
		}

		private void RequestForUpdatesSuccess (IDictionary _responseDict)
		{
			WebResponse			_response	= WebResponse.WebResponseOnSuccess(_responseDict);
			ProductUpdateInfo 	_updateInfo;

			if (_response.Status == 200)
			{
				_updateInfo = new ProductUpdateInfo(ProductVersion, _response.Data);
			}
			else
			{
				_updateInfo	= new ProductUpdateInfo(false);
			}

			// Process update info data
			OnReceivingUpdateInfo(_updateInfo);

			// Reset
			ResetFieldsRelatedToUpdateCheck();
		}

		private void RequestForUpdatesFailed (IDictionary _responseDict)
		{
			string _message	= string.Format(kCheckUpdatesFailedMessage, ProductName);

			// Show dialog
			if (!m_isAutomaticCheck)
			{
				ShowUpdatePrompt(ProductName, _message);
			}

			// Reset
			ResetFieldsRelatedToUpdateCheck();
		}
#endif

		#endregion

		#region Update Prompt Methods

#if UNITY_EDITOR
		private void OnReceivingUpdateInfo (ProductUpdateInfo _updateInfo)
		{
			// New update is not available
			if (!_updateInfo.NewUpdateAvailable)
			{
				if (!m_isAutomaticCheck)
				{
					string _uptoDateMessage	= string.Format(kAlreadyUptoDateMessage, ProductName);

					// Show update prompt dialog
					ShowUpdatePrompt(ProductName, _uptoDateMessage);
				}

				return;
			}
			
			// Cache update info
			m_productUpdateInfo				= _updateInfo;

			// User has already skipped download for this version
			string _versionNO				= m_productUpdateInfo.VersionNumber;

			if (m_isAutomaticCheck && EditorPrefs.GetBool(GetKeyForSkippedVersion(_versionNO), false))
			{
				return;
			}

			// New update is available
			string _updateAvailableMessage	= string.Format(kNewVersionAvailableMessage, ProductName);
			string _releaseNote				= "Release Note:\n\n" + _updateInfo.ReleaseNote;
			List<string> _buttonNames		= new List<string>();

			// Check if download from asset store is allowed
			if (!string.IsNullOrEmpty(_updateInfo.AssetStoreLink))
			{
				_buttonNames.Add(Constants.kButtonDownloadFromAssetStore);
			}

			// Check if download from our server is allowed
			if (!string.IsNullOrEmpty(_updateInfo.DownloadLink))
			{
				_buttonNames.Add(Constants.kButtonDownloadFromOurServer);
			}
			
			// It will have skip button
			_buttonNames.Add(Constants.kButtonSkipVersion);

			// Show update prompt dialog
			ShowUpdatePrompt(ProductName, _updateAvailableMessage, _releaseNote, _buttonNames.ToArray());
		}
	
		private void ShowUpdatePrompt (string _title, string _message, string _description = null, string[] _buttons = null)
		{
			if (m_promptWindow == null)
				m_promptWindow	= EditorWindow.CreateInstance<UpdatePromptWindow>();
			
			// Set properties
			#if !(UNITY_5_0) && (UNITY_5 || UNITY_6 || UNITY_7)
			m_promptWindow.titleContent			= new GUIContent(_title);
			#else
			m_promptWindow.title				= _title;
			#endif
			m_promptWindow.Message				= _message;
			m_promptWindow.Description			= _description;
			m_promptWindow.Buttons				= _buttons;
			m_promptWindow.LogoTexture			= LogoTexture;
			m_promptWindow.CallbackOnDismiss	= OnUpdatePromptDismissed;
			
			// Show window
			m_promptWindow.ShowUtility();
		}

		private void OnUpdatePromptDismissed (string _buttonName)
		{
			m_promptWindow	= null;

			if (_buttonName == null)
				return;

			// User pressed download from our server
			if (Constants.kButtonDownloadFromOurServer.Equals(_buttonName))
			{
				OpenLink(m_productUpdateInfo.DownloadLink);
				return;
			}

			// User pressed download from asset store
			if (Constants.kButtonDownloadFromAssetStore.Equals(_buttonName))
			{
				OpenLink(m_productUpdateInfo.AssetStoreLink);
				return;
			}

			// User presses skip this version
			if (Constants.kButtonSkipVersion.Equals(_buttonName))
			{
				EditorPrefs.SetBool(GetKeyForSkippedVersion(m_productUpdateInfo.VersionNumber), true);
				return;
			}
		}

		private string GetKeyForSkippedVersion (string _version)
		{
			return string.Format("{0}-{1}-{2}", GetGlobalIdentificationForThisProduct(), kSkippedVersionPrefix, _version);
		}

		private void OpenLink (string _link)
		{
			if (!string.IsNullOrEmpty(_link))
			{					
				Application.OpenURL(_link);
			}
		}

		private void ResetFieldsRelatedToUpdateCheck ()
		{
			m_isAutomaticCheck	= false;
			m_updateGETRequest	= null;
		}
#endif
		#endregion
	}
}