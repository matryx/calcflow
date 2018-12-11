using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace Nanome.Core
{

    public static class Macros
    {

#if DEBUG_BUILD
        public static readonly bool Debug = true;
#else
        public static readonly bool Debug = false;
#endif

#if UNITY_EDITOR
        public static readonly bool Editor = true;
#else
        public static readonly bool Editor = false;
#endif

#if ENTERPRISE_BUILD
        public static readonly bool Enterprise = true;
#else
        public static readonly bool Enterprise = false;
#endif

#if OCULUS
        public static readonly bool Oculus = true;
#else
        public static readonly bool Oculus = false;
#endif

#if !UNITY_EDITOR && !ENTERPRISE_BUILD && !DEBUG_BUILD
        public static readonly bool Logging = true;
#else
        public static readonly bool Logging = false;
#endif

        public static bool IsPlaying = false;
        public static bool IsDarkTheme = false;

        [RuntimeInitializeOnLoadMethod(RuntimeInitializeLoadType.BeforeSceneLoad)]
        private static void OnStartup()
        {
#if UNITY_EDITOR
            if (UnityEditor.EditorGUIUtility.isProSkin)
            {
                IsDarkTheme = true;
            }
            if (Application.isPlaying)
            {
                IsPlaying = true;
            }
#endif
        }

    }

}