using UnityEditor;
using UnityEngine;

namespace Crosstales.Common.EditorTask
{
    /// <summary>Copies all resources to 'Editor Default Resources'.</summary>
    [InitializeOnLoad]
    public abstract class SetupResources : BaseSetupResources
    {

        #region Constructor

        static SetupResources()
        {

#if !CT_DEVELOP
            string path = Application.dataPath;
            string assetpath = "Assets/Plugins/crosstales/Common/";

            string sourceFolder = path + "/Plugins/crosstales/Common/Icons/";
            string source = assetpath + "Icons/";

            string targetFolder = path + "/Editor Default Resources/crosstales/Common/";
            string target = "Assets/Editor Default Resources/crosstales/Common/";
            string metafile = assetpath + "Icons.meta";

            //Debug.Log(source + " - " + sourceFolder + " - " + target + " - " + targetFolder + " - " + metafile);
            setupResources(source, sourceFolder, target, targetFolder, metafile);
#endif
        }

        #endregion
    }
}
// © 2019 crosstales LLC (https://www.crosstales.com)