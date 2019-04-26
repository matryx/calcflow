// !!! BE CAREFUL !!!
// This file should stay in the Nanome/Core folder or it will break the build scripts

namespace Nanome.Core
{
    public static class Version
    {
        public const string Current = "1.04";
        public const string Notes = "EditorVersion";

        public static string GetVersion()
        {
            return Current + "-" + Notes;
        }
    }
}