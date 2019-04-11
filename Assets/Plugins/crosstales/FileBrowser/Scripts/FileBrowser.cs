using UnityEngine;

namespace Crosstales.FB
{
    /// <summary>Native file browser various actions like open file, open folder and save file.</summary>
    public class FileBrowser : MonoBehaviour
    {
        #region Variables

        private static Wrapper.IFileBrowser platformWrapper;

        #endregion


        #region Constructor

        static FileBrowser()
        {
#if UNITY_STANDALONE_WIN || UNITY_EDITOR_WIN
            if (Util.Helper.isEditor && !Util.Config.NATIVE_WINDOWS)
#else
            if (Util.Helper.isEditor)
#endif
            {
#if UNITY_EDITOR
                platformWrapper = new Wrapper.FileBrowserEditor();
#endif
            }
            else if (Util.Helper.isMacOSPlatform)
            {
#if UNITY_STANDALONE_OSX && !UNITY_EDITOR
                platformWrapper = new Wrapper.FileBrowserMac();
#endif
            }
            else if (Util.Helper.isWindowsPlatform)
            {
#if UNITY_STANDALONE_WIN || UNITY_EDITOR_WIN
                platformWrapper = new Wrapper.FileBrowserWindows();
#endif
            }
            else if (Util.Helper.isLinuxPlatform)
            {
#if UNITY_STANDALONE_LINUX && !UNITY_EDITOR
                platformWrapper = new Wrapper.FileBrowserLinux();
#endif
            }
            else
            {
                if (!Util.Constants.isPro && Util.Helper.isWSAPlatform)
                    Debug.LogWarning("<color=blue>File Browser PRO</color> supports <b>UWP (WSA)</b>. Please consider upgrading: <b>" + Util.Constants.ASSET_PRO_URL + "</b>");

                platformWrapper = new Wrapper.FileBrowserGeneric();
            }

            if (Util.Config.DEBUG)
                Debug.Log(platformWrapper);
        }

        #endregion


        #region Properties

        /// <summary>Indicates if this wrapper can open multiple files.</summary>
        /// <returns>Wrapper can open multiple files.</returns>
        public static bool canOpenMultipleFiles
        {
            get
            {
                return platformWrapper.canOpenMultipleFiles;
            }
        }

        /// <summary>Indicates if this wrapper can open multiple folders.</summary>
        /// <returns>Wrapper can open multiple folders.</returns>
        public static bool canOpenMultipleFolders
        {
            get
            {
                return platformWrapper.canOpenMultipleFolders;
            }
        }

        #endregion


        #region Public methods

        /// <summary>Open native file browser for a single file.</summary>
        /// <param name="extension">Allowed extension, e.g. "png" (optional)</param>
        /// <returns>Returns a string of the chosen file. Empty string when cancelled</returns>
        public static string OpenSingleFile(string extension = "*")
        {
            return OpenSingleFile(Util.Constants.TEXT_OPEN_FILE, string.Empty, getFilter(extension));
        }

        /// <summary>Open native file browser for a single file.</summary>
        /// <param name="title">Dialog title</param>
        /// <param name="directory">Root directory</param>
        /// <param name="extensions">Allowed extensions, e.g. "png" (optional)</param>
        /// <returns>Returns a string of the chosen file. Empty string when cancelled</returns>
        public static string OpenSingleFile(string title, string directory, params string[] extensions)
        {
            return OpenSingleFile(title, directory, getFilter(extensions));
        }

        /// <summary>Open native file browser for a single file.</summary>
        /// <param name="title">Dialog title</param>
        /// <param name="directory">Root directory</param>
        /// <param name="extensions">List of extension filters (optional)</param>
        /// <returns>Returns a string of the chosen file. Empty string when cancelled</returns>
        public static string OpenSingleFile(string title, string directory, params ExtensionFilter[] extensions)
        {
            return platformWrapper.OpenSingleFile(title, directory, extensions);
        }

        /// <summary>Open native file browser for multiple files.</summary>
        /// <param name="extension">Allowed extension, e.g. "png" (optional)</param>
        /// <returns>Returns a string of the chosen file. Empty string when cancelled</returns>
        public static string[] OpenFiles(string extension = "*")
        {
            return OpenFiles(Util.Constants.TEXT_OPEN_FILES, string.Empty, getFilter(extension));
        }

        /// <summary>Open native file browser for multiple files.</summary>
        /// <param name="title">Dialog title</param>
        /// <param name="directory">Root directory</param>
        /// <param name="extensions">Allowed extensions, e.g. "png" (optional)</param>
        /// <returns>Returns array of chosen files. Zero length array when cancelled</returns>
        public static string[] OpenFiles(string title, string directory, params string[] extensions)
        {
            return OpenFiles(title, directory, getFilter(extensions));
        }

        /// <summary>Open native file browser for multiple files.</summary>
        /// <param name="title">Dialog title</param>
        /// <param name="directory">Root directory</param>
        /// <param name="extensions">List of extension filters (optional)</param>
        /// <returns>Returns array of chosen files. Zero length array when cancelled</returns>
        public static string[] OpenFiles(string title, string directory, params ExtensionFilter[] extensions)
        {
            return platformWrapper.OpenFiles(title, directory, extensions, true);
        }

        /// <summary>Open native folder browser for a single folder.</summary>
        /// <returns>Returns a string of the chosen folder. Empty string when cancelled</returns>
        public static string OpenSingleFolder()
        {
            return OpenSingleFolder(Util.Constants.TEXT_OPEN_FOLDER);
        }

        /// <summary>Open native folder browser for a single folder.</summary>
        /// <param name="title">Dialog title</param>
        /// <param name="directory">Root directory (default: current, optional)</param>
        /// <returns>Returns a string of the chosen folder. Empty string when cancelled</returns>
        public static string OpenSingleFolder(string title, string directory = "")
        {
            return platformWrapper.OpenSingleFolder(title, directory);
        }

        /// <summary>
        /// Open native folder browser for multiple folders.
        /// NOTE: Multiple folder selection isnt't supported under Windows!
        /// </summary>
        /// <returns>Returns array of chosen folders. Zero length array when cancelled</returns>
        public static string[] OpenFolders()
        {
            return OpenFolders(Util.Constants.TEXT_OPEN_FOLDERS);
        }

        /// <summary>
        /// Open native folder browser for multiple folders.
        /// NOTE: Multiple folder selection isnt't supported on Windows!
        /// </summary>
        /// <param name="title">Dialog title</param>
        /// <param name="directory">Root directory (default: current, optional)</param>
        /// <returns>Returns array of chosen folders. Zero length array when cancelled</returns>
        public static string[] OpenFolders(string title, string directory = "")
        {
            return platformWrapper.OpenFolders(title, directory, true);
        }

        /// <summary>Open native save file browser</summary>
        /// <param name="defaultName">Default file name (optional)</param>
        /// <param name="extensions">File extensions, e.g. "png" (optional)</param>
        /// <returns>Returns chosen file. Empty string when cancelled</returns>
        public static string SaveFile(string defaultName = "", string extension = "*")
        {
            return SaveFile(Util.Constants.TEXT_SAVE_FILE, string.Empty, defaultName, getFilter(extension));
        }

        /// <summary>Open native save file browser</summary>
        /// <param name="title">Dialog title</param>
        /// <param name="directory">Root directory</param>
        /// <param name="defaultName">Default file name</param>
        /// <param name="extensions">File extensions, e.g. "png" (optional)</param>
        /// <returns>Returns chosen file. Empty string when cancelled</returns>
        public static string SaveFile(string title, string directory, string defaultName, params string[] extensions)
        {
            return SaveFile(title, directory, defaultName, getFilter(extensions));
        }

        /// <summary>Open native save file browser</summary>
        /// <param name="title">Dialog title</param>
        /// <param name="directory">Root directory</param>
        /// <param name="defaultName">Default file name</param>
        /// <param name="extensions">List of extension filters (optional)</param>
        /// <returns>Returns chosen file. Empty string when cancelled</returns>
        public static string SaveFile(string title, string directory, string defaultName, params ExtensionFilter[] extensions)
        {
            return platformWrapper.SaveFile(title, directory, string.IsNullOrEmpty(defaultName) ? Util.Constants.TEXT_SAVE_FILE_NAME : defaultName, extensions);
        }

        /// <summary>Open native file browser for multiple files.</summary>
        /// <param name="cb">Callback for the async operation.</param>
        /// <param name="multiselect">Allow multiple file selection (default: true, optional)</param>
        /// <param name="extensions">Allowed extensions, e.g. "png" (optional)</param>
        /// <returns>Returns array of chosen files. Zero length array when cancelled</returns>
        public static void OpenFilesAsync(System.Action<string[]> cb, bool multiselect = true, params string[] extensions)
        {
            OpenFilesAsync(cb, multiselect ? Util.Constants.TEXT_OPEN_FILES : Util.Constants.TEXT_OPEN_FILE, string.Empty, multiselect, getFilter(extensions));
        }

        /// <summary>Open native file browser for multiple files.</summary>
        /// <param name="cb">Callback for the async operation.</param>
        /// <param name="title">Dialog title</param>
        /// <param name="directory">Root directory</param>
        /// <param name="multiselect">Allow multiple file selection (default: true, optional)</param>
        /// <param name="extensions">Allowed extensions, e.g. "png" (optional)</param>
        /// <returns>Returns array of chosen files. Zero length array when cancelled</returns>
        public static void OpenFilesAsync(System.Action<string[]> cb, string title, string directory, bool multiselect = true, params string[] extensions)
        {
            OpenFilesAsync(cb, title, directory, multiselect, getFilter(extensions));
        }

        /// <summary>Open native file browser for multiple files (async).</summary>
        /// <param name="cb">Callback for the async operation.</param>
        /// <param name="title">Dialog title</param>
        /// <param name="directory">Root directory</param>
        /// <param name="multiselect">Allow multiple file selection (default: true, optional)</param>
        /// <param name="extensions">List of extension filters (optional)</param>
        /// <returns>Returns array of chosen files. Zero length array when cancelled</returns>
        public static void OpenFilesAsync(System.Action<string[]> cb, string title, string directory, bool multiselect = true, params ExtensionFilter[] extensions)
        {
            platformWrapper.OpenFilesAsync(title, directory, extensions, multiselect, cb);
        }

        /// <summary>Open native folder browser for multiple folders (async).</summary>
        /// <param name="cb">Callback for the async operation.</param>
        /// <param name="multiselect">Allow multiple folder selection (default: true, optional)</param>
        /// <returns>Returns array of chosen folders. Zero length array when cancelled</returns>
        public static void OpenFoldersAsync(System.Action<string[]> cb, bool multiselect = true)
        {
            OpenFoldersAsync(cb, Util.Constants.TEXT_OPEN_FOLDERS, string.Empty, multiselect);
        }

        /// <summary>Open native folder browser for multiple folders (async).</summary>
        /// <param name="cb">Callback for the async operation.</param>
        /// <param name="title">Dialog title</param>
        /// <param name="directory">Root directory (default: current, optional)</param>
        /// <param name="multiselect">Allow multiple folder selection (default: true, optional)</param>
        /// <returns>Returns array of chosen folders. Zero length array when cancelled</returns>
        public static void OpenFoldersAsync(System.Action<string[]> cb, string title, string directory = "", bool multiselect = true)
        {
            platformWrapper.OpenFoldersAsync(title, directory, multiselect, cb);
        }

        /// <summary>Open native save file browser</summary>
        /// <param name="cb">Callback for the async operation.</param>
        /// <param name="defaultName">Default file name (optional)</param>
        /// <param name="extension">File extension, e.g. "png" (optional)</param>
        /// <returns>Returns chosen file. Empty string when cancelled</returns>
        public static void SaveFileAsync(System.Action<string> cb, string defaultName = "", string extension = "*")
        {
            SaveFileAsync(cb, Util.Constants.TEXT_SAVE_FILE, string.Empty, defaultName, getFilter(extension));
        }

        /// <summary>Open native save file browser</summary>
        /// <param name="cb">Callback for the async operation.</param>
        /// <param name="title">Dialog title</param>
        /// <param name="directory">Root directory</param>
        /// <param name="defaultName">Default file name</param>
        /// <param name="extensions">File extensions, e.g. "png" (optional)</param>
        /// <returns>Returns chosen file. Empty string when cancelled</returns>
        public static void SaveFileAsync(System.Action<string> cb, string title, string directory, string defaultName, params string[] extensions)
        {
            SaveFileAsync(cb, title, directory, defaultName, getFilter(extensions));
        }

        /// <summary>Open native save file browser (async).</summary>
        /// <param name="cb">Callback for the async operation.</param>
        /// <param name="title">Dialog title</param>
        /// <param name="directory">Root directory</param>
        /// <param name="defaultName">Default file name</param>
        /// <param name="extensions">List of extension filters (optional)</param>
        /// <returns>Returns chosen file. Empty string when cancelled</returns>
        public static void SaveFileAsync(System.Action<string> cb, string title, string directory, string defaultName, params ExtensionFilter[] extensions)
        {
            platformWrapper.SaveFileAsync(title, directory, string.IsNullOrEmpty(defaultName) ? Util.Constants.TEXT_SAVE_FILE_NAME : defaultName, extensions, cb);
        }

        /// <summary>
        /// Find files inside a path.
        /// </summary>
        /// <param name="path">Path to find the files</param>
        /// <param name="isRecursive">Recursive search (default: false, optional)</param>
        /// <param name="extensions">Extensions for the file search, e.g. "png" (optional)</param>
        /// <returns>Returns array of the found files inside the path (alphabetically ordered). Zero length array when an error occured.</returns>
        public static string[] GetFiles(string path, bool isRecursive = false, params string[] extensions)
        {
            return Util.Helper.GetFiles(path, isRecursive, extensions);
        }

        /// <summary>
        /// Find files inside a path.
        /// </summary>
        /// <param name="path">Path to find the files</param>
        /// <param name="isRecursive">Recursive search</param>
        /// <param name="extensions">List of extension filters for the search (optional)</param>
        /// <returns>Returns array of the found files inside the path. Zero length array when an error occured.</returns>
        public static string[] GetFiles(string path, bool isRecursive, params ExtensionFilter[] extensions)
        {
            System.Collections.Generic.List<string> exts = new System.Collections.Generic.List<string>();

            foreach (ExtensionFilter extensionFilter in extensions)
            {
                foreach (string ext in extensionFilter.Extensions)
                {
                    exts.Add(ext);
                }
            }

            return GetFiles(path, isRecursive, exts.ToArray());
        }

        /// <summary>
        /// Find directories inside.
        /// </summary>
        /// <param name="path">Path to find the directories</param>
        /// <param name="isRecursive">Recursive search (default: false, optional)</param>
        /// <returns>Returns array of the found directories inside the path. Zero length array when an error occured.</returns>
        public static string[] GetDirectories(string path, bool isRecursive = false)
        {
            return Util.Helper.GetDirectories(path, isRecursive);
        }

        #endregion


        #region Private methods

        private static ExtensionFilter[] getFilter(params string[] extensions)
        {
            if (extensions != null && extensions.Length > 0)
            {
                ExtensionFilter[] filter = new ExtensionFilter[extensions.Length];

                string extension;

                for (int ii = 0; ii < extensions.Length; ii++)
                {
                    extension = string.IsNullOrEmpty(extensions[ii]) ? "*" : extensions[ii];
                    filter[ii] = new ExtensionFilter(extension.Equals("*") ? Util.Constants.TEXT_ALL_FILES : extension, extension);
                }

                if (Util.Config.DEBUG)
                    Debug.Log("getFilter: " + filter.CTDump());

                return filter;
            }

            return null;
        }

        #endregion
    }

    /// <summary>Filter for extensions.</summary>
    public struct ExtensionFilter
    {
        public string Name;
        public string[] Extensions;

        public ExtensionFilter(string filterName, params string[] filterExtensions)
        {
            Name = filterName;
            Extensions = filterExtensions;
        }

        public override string ToString()
        {
            System.Text.StringBuilder result = new System.Text.StringBuilder();

            result.Append(GetType().Name);
            result.Append(Util.Constants.TEXT_TOSTRING_START);

            result.Append("Name='");
            result.Append(Name);
            result.Append(Util.Constants.TEXT_TOSTRING_DELIMITER);

            result.Append("Extensions='");
            result.Append(Extensions.CTDump());
            result.Append(Util.Constants.TEXT_TOSTRING_DELIMITER_END);

            result.Append(Util.Constants.TEXT_TOSTRING_END);

            return result.ToString();
        }
    }
}
// © 2017-2019 crosstales LLC (https://www.crosstales.com)