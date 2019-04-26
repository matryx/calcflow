using UnityEditor;
using UnityEngine;

namespace Crosstales.Common.EditorTask
{
    /// <summary>Base for copying all resources to 'Editor Default Resources'.</summary>
    public abstract class BaseSetupResources
    {

        protected static void setupResources(string source, string sourceFolder, string target, string targetFolder, string metafile)
        {
            bool exists = false;

            try
            {
                if (System.IO.Directory.Exists(sourceFolder))
                {
                    exists = true;

                    if (!System.IO.Directory.Exists(targetFolder))
                    {
                        System.IO.Directory.CreateDirectory(targetFolder);
                    }

                    var dirSource = new System.IO.DirectoryInfo(sourceFolder);

                    foreach (System.IO.FileInfo file in dirSource.GetFiles("*"))
                    {
                        if (System.IO.File.Exists(targetFolder + file.Name))
                        {
                            if (Util.BaseConstants.DEV_DEBUG)
                                Debug.Log("File exists: " + file);
                        }
                        else
                        {
                            AssetDatabase.MoveAsset(source + file.Name, target + file.Name);

                            if (Util.BaseConstants.DEV_DEBUG)
                                Debug.Log("File moved: " + file);
                        }
                    }

                    //dirSource.Delete(true);
                    dirSource.Delete();

                    if (System.IO.File.Exists(metafile))
                    {
                        System.IO.File.Delete(metafile);
                    }
                }
            }
            catch (System.Exception ex)
            {
                if (Util.BaseConstants.DEV_DEBUG)
                    Debug.LogError("Could not move all files: " + ex);
            }
            finally
            {
                if (exists)
                    EditorUtil.BaseEditorHelper.RefreshAssetDatabase();
            }
        }
    }
}
// © 2018-2019 crosstales LLC (https://www.crosstales.com)