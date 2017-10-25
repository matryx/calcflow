using UnityEngine;
#if UNITY_EDITOR
using UnityEditor;
using UnityEditor.Callbacks;
#endif
using System.IO;

namespace VoxelBusters.ThirdParty.XUPorter
{
	public static class XCodePostProcess
	{

#if UNITY_EDITOR
		[PostProcessBuild(999)]
		public static void OnPostProcessBuild( BuildTarget target, string pathToBuiltProject )
		{
			string 	_targetStr	= target.ToString();

			if (!(_targetStr.Equals("iOS") || _targetStr.Equals("iPhone")))
			{
				Debug.LogWarning("Target is not iPhone. XCodePostProcess will not run");
				return;
			}

			// Create a new project object from build target
			XCProject project = new XCProject( pathToBuiltProject );

			// Find and run through all xcodemods files to patch the project.
			// Please pay attention that ALL xcodemods files in your project folder will be excuted!
			string[] files = Directory.GetFiles( Utility.AssetsUtility.GetProjectPath(), "*.xcodemods", SearchOption.AllDirectories );
			foreach( string file in files ) {
				UnityEngine.Debug.Log("Xcodemods File: "+file);
				project.ApplyMod( file );
			}

			//TODO implement generic settings as a module option
	//		project.overwriteBuildSetting("CODE_SIGN_IDENTITY[sdk=iphoneos*]", "iPhone Distribution", "Release");
			
			// Finally save the xcode project
			project.Save();

		}
#endif

		public static void Log(string message)
		{
			UnityEngine.Debug.Log("PostProcess: "+message);
		}
	}
}