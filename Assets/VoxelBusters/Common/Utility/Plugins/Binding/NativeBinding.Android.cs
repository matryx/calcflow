using UnityEngine;
using System.Collections;

#if UNITY_ANDROID
namespace VoxelBusters.Utility
{
	public partial class NativeBinding : MonoBehaviour 
	{
		#region Platform Native Info
		
		class NativeInfo
		{
			// Handler class name
			public class Class
			{
				public const string NAME			= "com.voxelbusters.utility.NativePlatformInfo";
			}
			
			// For holding method names
			public class Methods
			{
				public const string GET_BUILD_IDENTIFIER	= "getPackageName";
				public const string GET_BUILD_VERSION		= "getPackageVersionName";
			}
		}
		
		#endregion
		
		#region  Required Variables
		
		private static AndroidJavaClass 	m_nativeBinding;
		private static AndroidJavaClass  	PluginNativeBinding
		{
			get 
			{ 
				if(m_nativeBinding == null)
				{
					m_nativeBinding = new AndroidJavaClass(NativeInfo.Class.NAME);
				}
				return m_nativeBinding; 
			}
			
			set
			{
				m_nativeBinding = value;
			}
		}
		
		#endregion
	}
}
#endif