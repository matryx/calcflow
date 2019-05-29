using UnityEngine;
using UnityEditor;

namespace Crosstales.FB.EditorUtil
{
    /// <summary>Editor helper class.</summary>
    public abstract class EditorHelper : Common.EditorUtil.BaseEditorHelper
    {
        #region Static variables

        private static Texture2D logo_asset;
        private static Texture2D logo_asset_small;

        #endregion


        #region Static properties

        public static Texture2D Logo_Asset
        {
            get
            {
                if (Util.Constants.isPro)
                {
                    return loadImage(ref logo_asset, "logo_asset_pro.png");
                }
                else
                {
                    return loadImage(ref logo_asset, "logo_asset.png");
                }
            }
        }

        public static Texture2D Logo_Asset_Small
        {
            get
            {
                if (Util.Constants.isPro)
                {
                    return loadImage(ref logo_asset_small, "logo_asset_small_pro.png");
                }
                else
                {
                    return loadImage(ref logo_asset_small, "logo_asset_small.png");
                }
            }
        }

        #endregion


        #region Static methods

        /// <summary>Shows a banner for "File Browser PRO".</summary>
        public static void BannerFB()
        {
            if (!Util.Constants.isPro && Util.Constants.SHOW_FB_BANNER)
            {
                GUILayout.BeginHorizontal();
                {
                    EditorGUILayout.HelpBox("'File Browser PRO' is not installed!" + System.Environment.NewLine + "For UWP (WSA), extended support, PlayMaker actions and helping the project to be kept alive, please get it from the Unity AssetStore.", MessageType.Info);

                    GUILayout.BeginVertical(GUILayout.Width(32));
                    {
                        GUILayout.Space(4);

                        if (GUILayout.Button(new GUIContent(string.Empty, EditorHelper.Logo_Asset_FB, "Visit File Browser PRO in the Unity AssetStore")))
                        {
                            Application.OpenURL(Util.Constants.ASSET_FB);
                        }
                    }
                    GUILayout.EndVertical();
                }
                GUILayout.EndHorizontal();
            }
        }

        /// <summary>Loads an image as Texture2D from 'Editor Default Resources'.</summary>
        /// <param name="logo">Logo to load.</param>
        /// <param name="fileName">Name of the image.</param>
        /// <returns>Image as Texture2D from 'Editor Default Resources'.</returns>
        private static Texture2D loadImage(ref Texture2D logo, string fileName)
        {
            if (logo == null)
            {
#if CT_DEVELOP
                logo = (Texture2D)AssetDatabase.LoadAssetAtPath("Assets" + EditorConfig.ASSET_PATH + "Icons/" + fileName, typeof(Texture2D));
#else
                logo = (Texture2D)EditorGUIUtility.Load("crosstales/FileBrowser/" + fileName);
#endif

                if (logo == null)
                {
                    Debug.LogWarning("Image not found: " + fileName);
                }
            }

            return logo;
        }

        #endregion
    }
}
// © 2019 crosstales LLC (https://www.crosstales.com)