using UnityEditor;
using UnityEngine;

namespace Crosstales.FB.EditorTask
{
    /// <summary>Sets the required .NET API level.</summary>
    [InitializeOnLoad]
    public static class SetApiLevel
    {

        #region Constructor

        static SetApiLevel()
        {
            BuildTarget target = EditorUserBuildSettings.activeBuildTarget;
            BuildTargetGroup group = BuildPipeline.GetBuildTargetGroup(target);

#if UNITY_2018_2_OR_NEWER
            ApiCompatibilityLevel level = ApiCompatibilityLevel.NET_4_6;
#else
            ApiCompatibilityLevel level = ApiCompatibilityLevel.NET_2_0;
#endif
#if UNITY_STANDALONE_WIN
            ScriptingImplementation scriptingBackend = ScriptingImplementation.Mono2x;
#else
            ScriptingImplementation scriptingBackend = PlayerSettings.GetScriptingBackend(group);
#endif

            //Debug.Log("API level: " + PlayerSettings.GetApiCompatibilityLevel(group));
            //Debug.Log("Scripting backend: " + PlayerSettings.GetScriptingBackend(group));

            if (PlayerSettings.GetApiCompatibilityLevel(group) != level || PlayerSettings.GetScriptingBackend(group) != scriptingBackend)
            {
                PlayerSettings.SetApiCompatibilityLevel(group, level);
                PlayerSettings.SetScriptingBackend(group, scriptingBackend);
                Debug.Log("API level changed to '" + level + "' (" + scriptingBackend + ")");
            }
        }

#endregion
    }
}
// © 2017-2019 crosstales LLC (https://www.crosstales.com)