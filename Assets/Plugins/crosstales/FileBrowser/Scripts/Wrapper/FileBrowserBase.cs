using UnityEngine;

namespace Crosstales.FB.Wrapper
{
    /// <summary>Base class for all file browsers.</summary>
    public abstract class FileBrowserBase : IFileBrowser
    {

        #region Implemented methods

        public abstract bool canOpenMultipleFiles
        {
            get;
        }

        public abstract bool canOpenMultipleFolders
        {
            get;
        }

        public string OpenSingleFile(string title, string directory, ExtensionFilter[] extensions)
        {
            string[] files = OpenFiles(title, directory, extensions, false);
            string file = files.Length > 0 ? files[0] : string.Empty;

            return file;
        }

        public abstract string[] OpenFiles(string title, string directory, ExtensionFilter[] extensions, bool multiselect);

        public string OpenSingleFolder(string title, string directory)
        {
            string[] folders = OpenFolders(title, directory, false);
            return folders.Length > 0 ? folders[0] : string.Empty;
        }

        public abstract string[] OpenFolders(string title, string directory, bool multiselect);

        public abstract string SaveFile(string title, string directory, string defaultName, ExtensionFilter[] extensions);

        public abstract void OpenFilesAsync(string title, string directory, ExtensionFilter[] extensions, bool multiselect, System.Action<string[]> cb);

        public abstract void OpenFoldersAsync(string title, string directory, bool multiselect, System.Action<string[]> cb);

        public abstract void SaveFileAsync(string title, string directory, string defaultName, ExtensionFilter[] extensions, System.Action<string> cb);

        #endregion
    }
}
// © 2017-2019 crosstales LLC (https://www.crosstales.com)