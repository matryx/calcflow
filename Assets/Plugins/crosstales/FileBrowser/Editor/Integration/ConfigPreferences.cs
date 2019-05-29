using UnityEditor;
using UnityEngine;
using Crosstales.FB.EditorUtil;

namespace Crosstales.FB.EditorIntegration
{
    /// <summary>Unity "Preferences" extension.</summary>
    public class ConfigPreferences : ConfigBase
    {

        #region Variables

        private static int tab = 0;
        private static int lastTab = 0;
        private static ConfigPreferences cp;

        #endregion


        #region Static methods

        [PreferenceItem(Util.Constants.ASSET_NAME_SHORT)]
        private static void PreferencesGUI()
        {
            if (cp == null)
            {
                cp = ScriptableObject.CreateInstance(typeof(ConfigPreferences)) as ConfigPreferences;
            }

            tab = GUILayout.Toolbar(tab, new string[] { "Configuration", "Help", "About" });

            if (tab != lastTab)
            {
                lastTab = tab;
                GUI.FocusControl(null);
            }

            if (tab == 0)
            {
                cp.showConfiguration();

                EditorHelper.SeparatorUI();

                if (GUILayout.Button(new GUIContent(" Reset", EditorHelper.Icon_Reset, "Resets the configuration settings for this project.")))
                {
                    if (EditorUtility.DisplayDialog("Reset configuration?", "Reset the configuration of " + Util.Constants.ASSET_NAME + "?", "Yes", "No"))
                    {
                        Util.Config.Reset();
                        EditorConfig.Reset();
                        save();
                    }
                }

                GUILayout.Space(6);
            }
            else if (tab == 1)
            {
                cp.showHelp();
            }
            else
            {
                cp.showAbout();
            }

            if (GUI.changed)
            {
                save();
            }
        }

        #endregion
    }
}
// © 2019 crosstales LLC (https://www.crosstales.com)