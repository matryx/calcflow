namespace Crosstales.FB.Wrapper
{
    /// <summary>Interface for all file browsers.</summary>
    public interface IFileBrowser
    {


        #region Properties

        /// <summary>Indicates if this wrapper can open multiple files.</summary>
        /// <returns>Wrapper can open multiple files.</returns>
        bool canOpenMultipleFiles
        {
            get;
        }

        /// <summary>Indicates if this wrapper can open multiple folders.</summary>
        /// <returns>Wrapper can open multiple folders.</returns>
        bool canOpenMultipleFolders
        {
            get;
        }

        #endregion


        #region Methods

        /// <summary>Open native file browser for a single file.</summary>
        /// <param name="title">Dialog title</param>
        /// <param name="directory">Root directory</param>
        /// <param name="extensions">List of extension filters. Filter Example: new ExtensionFilter("Image Files", "jpg", "png")</param>
        /// <returns>Returns a string of the chosen file. Empty string when cancelled</returns>
        string OpenSingleFile(string title, string directory, ExtensionFilter[] extensions);

        /// <summary>Open native file browser for multiple files.</summary>
        /// <param name="title">Dialog title</param>
        /// <param name="directory">Root directory</param>
        /// <param name="extensions">List of extension filters. Filter Example: new ExtensionFilter("Image Files", "jpg", "png")</param>
        /// <param name="multiselect">Allow multiple file selection</param>
        /// <returns>Returns array of chosen files. Zero length array when cancelled</returns>
        string[] OpenFiles(string title, string directory, ExtensionFilter[] extensions, bool multiselect);

        /// <summary>Open native folder browser for a single folder.</summary>
        /// <param name="title">Dialog title</param>
        /// <param name="directory">Root directory</param>
        /// <returns>Returns a string of the chosen folder. Empty string when cancelled</returns>
        string OpenSingleFolder(string title, string directory);

        /// <summary>Open native folder browser for multiple folders.</summary>
        /// <param name="title">Dialog title</param>
        /// <param name="directory">Root directory</param>
        /// <param name="multiselect">Allow multiple folder selection</param>
        /// <returns>Returns array of chosen folders. Zero length array when cancelled</returns>
        string[] OpenFolders(string title, string directory, bool multiselect);

        /// <summary>Open native save file browser.</summary>
        /// <param name="title">Dialog title</param>
        /// <param name="directory">Root directory</param>
        /// <param name="defaultName">Default file name</param>
        /// <param name="extensions">List of extension filters. Filter Example: new ExtensionFilter("Image Files", "jpg", "png")</param>
        /// <returns>Returns chosen file. Empty string when cancelled</returns>
        string SaveFile(string title, string directory, string defaultName, ExtensionFilter[] extensions);

        /*
                /// <summary>Open native file browser for a single file (async).</summary>
                /// <param name="title">Dialog title</param>
                /// <param name="directory">Root directory</param>
                /// <param name="extensions">List of extension filters. Filter Example: new ExtensionFilter("Image Files", "jpg", "png")</param>
                /// <param name="cb">Callback for the async operation.</param>
                /// <returns>Returns a string of the chosen file. Empty string when cancelled</returns>
                void OpenSingleFileAsync(string title, string directory, ExtensionFilter[] extensions, Action<string> cb);
                
        */

        /// <summary>Open native file browser for multiple files (async).</summary>
        /// <param name="title">Dialog title</param>
        /// <param name="directory">Root directory</param>
        /// <param name="extensions">List of extension filters. Filter Example: new ExtensionFilter("Image Files", "jpg", "png")</param>
        /// <param name="multiselect">Allow multiple file selection</param>
        /// <param name="cb">Callback for the async operation.</param>
        /// <returns>Returns array of chosen files. Zero length array when cancelled</returns>
        void OpenFilesAsync(string title, string directory, ExtensionFilter[] extensions, bool multiselect, System.Action<string[]> cb);

        /*
                /// <summary>Open native folder browser for a single folder (async).</summary>
                /// <param name="title"></param>
                /// <param name="directory">Root directory</param>
                /// <param name="cb">Callback for the async operation.</param>
                /// <returns>Returns a string of the chosen folder. Empty string when cancelled</returns>
                void OpenSingleFolderAsync(string title, string directory, Action<string> cb);
        */

        /// <summary>Open native folder browser for multiple folders (async).</summary>
        /// <param name="title">Dialog title</param>
        /// <param name="directory">Root directory</param>
        /// <param name="multiselect">Allow multiple folder selection</param>
        /// <param name="cb">Callback for the async operation.</param>
        /// <returns>Returns array of chosen folders. Zero length array when cancelled</returns>
        void OpenFoldersAsync(string title, string directory, bool multiselect, System.Action<string[]> cb);

        /// <summary>Open native save file browser (async).</summary>
        /// <param name="title">Dialog title</param>
        /// <param name="directory">Root directory</param>
        /// <param name="defaultName">Default file name</param>
        /// <param name="extensions">List of extension filters. Filter Example: new ExtensionFilter("Image Files", "jpg", "png")</param>
        /// <param name="cb">Callback for the async operation.</param>
        /// <returns>Returns chosen file. Empty string when cancelled</returns>
        void SaveFileAsync(string title, string directory, string defaultName, ExtensionFilter[] extensions, System.Action<string> cb);

        #endregion
    }
}
// © 2018-2019 crosstales LLC (https://www.crosstales.com)