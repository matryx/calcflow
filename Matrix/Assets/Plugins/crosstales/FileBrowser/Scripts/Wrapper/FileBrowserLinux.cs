#if (UNITY_STANDALONE_LINUX && !UNITY_EDITOR) || CT_ENABLED
using System;
using System.Runtime.InteropServices;
using UnityEngine;

namespace Crosstales.FB.Wrapper
{
    /// <summary>File browser implementation for Linux (GTK).</summary>
    public class FileBrowserLinux : FileBrowserBase
    {
        #region Variables

        private static Action<string[]> _openFileCb;
        private static Action<string[]> _openFolderCb;
        private static Action<string> _saveFileCb;

        [UnmanagedFunctionPointer(CallingConvention.StdCall)]
        public delegate void AsyncCallback(string path);

        [DllImport("StandaloneFileBrowser")]
        private static extern void DialogInit();

        [DllImport("StandaloneFileBrowser")]
        private static extern IntPtr DialogOpenFilePanel(string title, string directory, string extension, bool multiselect);

        [DllImport("StandaloneFileBrowser")]
        private static extern IntPtr DialogOpenFolderPanel(string title, string directory, bool multiselect);

        [DllImport("StandaloneFileBrowser")]
        private static extern IntPtr DialogSaveFilePanel(string title, string directory, string defaultName, string extension);

        [DllImport("StandaloneFileBrowser")]
        private static extern void DialogOpenFilePanelAsync(string title, string directory, string extension, bool multiselect, AsyncCallback callback);

        [DllImport("StandaloneFileBrowser")]
        private static extern void DialogOpenFolderPanelAsync(string title, string directory, bool multiselect, AsyncCallback callback);

        [DllImport("StandaloneFileBrowser")]
        private static extern void DialogSaveFilePanelAsync(string title, string directory, string defaultName, string extension, AsyncCallback callback);

        private const char splitChar = (char)28;

        #endregion


        #region Constructor

        public FileBrowserLinux()
        {
            DialogInit();
        }

        #endregion


        #region Implemented methods

        public override bool canOpenMultipleFiles
        {
            get
            {
                return true;
            }
        }

        public override bool canOpenMultipleFolders
        {
            get
            {
                return true;
            }
        }

        public override string[] OpenFiles(string title, string directory, ExtensionFilter[] extensions, bool multiselect)
        {
            string paths = Marshal.PtrToStringAnsi(DialogOpenFilePanel(title, directory, getFilterFromFileExtensionList(extensions), multiselect));
            return paths.Split(splitChar);
        }

        public override string[] OpenFolders(string title, string directory, bool multiselect)
        {
            string paths = Marshal.PtrToStringAnsi(DialogOpenFolderPanel(title, directory, multiselect));
            return paths.Split(splitChar);
        }

        public override string SaveFile(string title, string directory, string defaultName, ExtensionFilter[] extensions)
        {
            return Marshal.PtrToStringAnsi(DialogSaveFilePanel(title, directory, defaultName, getFilterFromFileExtensionList(extensions)));
        }

        public override void OpenFilesAsync(string title, string directory, ExtensionFilter[] extensions, bool multiselect, Action<string[]> cb)
        {
            _openFileCb = cb;
            DialogOpenFilePanelAsync(
                title,
                directory,
                getFilterFromFileExtensionList(extensions),
                multiselect,
                (string result) => { _openFileCb.Invoke(result.Split(splitChar)); });
        }

        public override void OpenFoldersAsync(string title, string directory, bool multiselect, Action<string[]> cb)
        {
            _openFolderCb = cb;
            DialogOpenFolderPanelAsync(
                title,
                directory,
                multiselect,
                (string result) => { _openFolderCb.Invoke(result.Split(splitChar)); });
        }

        public override void SaveFileAsync(string title, string directory, string defaultName, ExtensionFilter[] extensions, Action<string> cb)
        {
            _saveFileCb = cb;
            DialogSaveFilePanelAsync(
                title,
                directory,
                defaultName,
                getFilterFromFileExtensionList(extensions),
                (string result) => { _saveFileCb.Invoke(result); });
        }

        #endregion


        #region Private methods

        private static string getFilterFromFileExtensionList(ExtensionFilter[] extensions)
        {
            if (extensions == null)
            {
                return string.Empty;
            }

            System.Text.StringBuilder sb = new System.Text.StringBuilder();
            ExtensionFilter filter;

            for (int xx = 0; xx < extensions.Length; xx++)
            {
                filter = extensions[xx];

                sb.Append(filter.Name);
                sb.Append(";");

                for (int ii = 0; ii < filter.Extensions.Length; ii++)
                {
                    sb.Append(filter.Extensions[ii]);

                    if (ii + 1 < filter.Extensions.Length)
                        sb.Append(",");
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
// © 2019 crosstales LLC (https://www.crosstales.com)