#if UNITY_STANDALONE_WIN || UNITY_EDITOR_WIN || CT_ENABLED
using UnityEngine;
using System;

namespace Crosstales.FB.Wrapper
{
    // For fullscreen support:
    // - "PlayerSettings/Visible In Background" should be enabled, otherwise when file dialog opened, app window minimizes automatically.

    /// <summary>File browser implementation for Windows.</summary>
    public class FileBrowserWindows : FileBrowserBase
    {
        #region Implemented methods

        public override bool canOpenMultipleFiles
        {
            get
            {
                return FileBrowserWinImpl.canOpenMultipleFiles;
            }
        }

        public override bool canOpenMultipleFolders
        {
            get
            {
                return FileBrowserWinImpl.canOpenMultipleFolders;
            }
        }

        public override string[] OpenFiles(string title, string directory, ExtensionFilter[] extensions, bool multiselect)
        {

            return FileBrowserWinImpl.OpenFiles(title, directory, getFilterFromFileExtensionList(extensions), multiselect);
        }

        public override string[] OpenFolders(string title, string directory, bool multiselect)
        {
            if (multiselect)
                Debug.LogWarning("'multiselect' for folders is not supported under Windows.");

            return FileBrowserWinImpl.OpenFolder(title, directory);
        }

        public override string SaveFile(string title, string directory, string defaultName, ExtensionFilter[] extensions)
        {
            return FileBrowserWinImpl.SaveFile(title, directory, defaultName, getFilterFromFileExtensionList(extensions));
        }

        public override void OpenFilesAsync(string title, string directory, ExtensionFilter[] extensions, bool multiselect, Action<string[]> cb)
        {
            cb.Invoke(OpenFiles(title, directory, extensions, multiselect));
        }

        public override void OpenFoldersAsync(string title, string directory, bool multiselect, Action<string[]> cb)
        {
            cb.Invoke(OpenFolders(title, directory, multiselect));
        }

        public override void SaveFileAsync(string title, string directory, string defaultName, ExtensionFilter[] extensions, Action<string> cb)
        {
            cb.Invoke(SaveFile(title, directory, defaultName, extensions));
        }

        #endregion


        #region Private methods

        private static string getFilterFromFileExtensionList(ExtensionFilter[] extensions)
        {
            System.Text.StringBuilder sb = new System.Text.StringBuilder();
            ExtensionFilter filter;

            for (int xx = 0; xx < extensions.Length; xx++)
            {
                filter = extensions[xx];

                sb.Append(filter.Name);
                sb.Append(" (");

                for (int ii = 0; ii < filter.Extensions.Length; ii++)
                {
                    sb.Append("*.");
                    sb.Append(filter.Extensions[ii]);

                    if (ii + 1 < filter.Extensions.Length)
                        sb.Append(",");
                }

                sb.Append(")|");

                for (int ii = 0; ii < filter.Extensions.Length; ii++)
                {
                    sb.Append("*.");
                    sb.Append(filter.Extensions[ii]);

                    if (ii + 1 < filter.Extensions.Length)
                        sb.Append(";");
                }

                if (xx + 1 < extensions.Length)
                    sb.Append("|");
            }

            if (Util.Config.DEBUG)
                Debug.Log("getFilterFromFileExtensionList: " + sb.ToString());

            return sb.ToString();
        }

        #endregion
    }
}
#endif
// © 2017-2019 crosstales LLC (https://www.crosstales.com)