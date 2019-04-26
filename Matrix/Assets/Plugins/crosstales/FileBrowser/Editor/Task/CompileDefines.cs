using UnityEditor;

namespace Crosstales.FB.EditorTask
{
    /// <summary>Adds the given define symbols to PlayerSettings define symbols.</summary>
    [InitializeOnLoad]
    public class CompileDefines : Common.EditorTask.BaseCompileDefines
    {

        private static readonly string symbol = "CT_FB";

        static CompileDefines()
        {
            addSymbolsToAllTargets(symbol);
        }
    }
}
// © 2017-2019 crosstales LLC (https://www.crosstales.com)