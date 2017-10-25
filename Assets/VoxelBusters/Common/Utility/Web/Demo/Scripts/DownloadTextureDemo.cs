using UnityEngine;
using System.Collections;
using DownloadTexture = VoxelBusters.Utility.DownloadTexture;

namespace VoxelBusters.Utility
{
	public class DownloadTextureDemo : MonoBehaviour 
	{
		#region Properties

		[SerializeField]
		private string 			m_URLString;

		[SerializeField]
		private MeshRenderer 	m_renderer;

		#endregion

		#region Methods

		public void StartDownload ()
		{
			URL _URL;

			// Check type of url
			if (m_URLString.StartsWith("http"))
				_URL = URL.URLWithString(m_URLString);
			else
				_URL = URL.FileURLWithPath(m_URLString);

			// Download image from given path
			DownloadTexture _newDownload	= new DownloadTexture(_URL, true, true);
			_newDownload.OnCompletion		= (Texture2D _texture, string _error)=>{

				Debug.Log(string.Format("[DownloadTextureDemo] Texture download completed. Error= {0}.", _error.GetPrintableString()));

				if (_texture != null)
					m_renderer.sharedMaterial.mainTexture	= _texture;
			};
			
			// Start download
			_newDownload.StartRequest();
		}

		#endregion
	}
}