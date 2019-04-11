using UnityEngine;
using System;

namespace Crosstales.FB.Wrapper
{
    /// <summary>File browser implementation for generic devices (currently NOT IMPLEMENTED).</summary>
    public class FileBrowserGeneric : FileBrowserBase
    {
        #region Implemented methods

        public override bool canOpenMultipleFiles
        {
            get
            {
                return false;
            }
        }

        public override bool canOpenMultipleFolders
        {
            get
            {
                return false;
            }
        }

        public override string[] OpenFiles(string title, string directory, ExtensionFilter[] extensions, bool multiselect)
        {
            Debug.LogWarning("'OpenFilePanel' is currently not supported for the current platform!");
            return new string[0];
        }

        public override string[] OpenFolders(string title, string directory, bool multiselect)
        {
            Debug.LogWarning("'OpenFolderPanel' is currently not supported for the current platform!");
            return new string[0];
        }

        public override string SaveFile(string title, string directory, string defaultName, ExtensionFilter[] extensions)
        {
            Debug.LogWarning("'SaveFilePanel' is currently not supported for the current platform!");
            return string.Empty;
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
    }
}
// © 2017-2019 crosstales LLC (https://www.crosstales.com)