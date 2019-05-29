using System.Collections.Generic;
using System.Linq;
using UnityEditor;
using UnityEngine;

namespace Crosstales.Common.EditorTask
{
    /// <summary>Base for adding and removing the given define symbols to PlayerSettings define symbols.</summary>
    public abstract class BaseCompileDefines
    {
        protected static void addSymbolsToAllTargets(params string[] symbols)
        {
            foreach (BuildTargetGroup group in System.Enum.GetValues(typeof(BuildTargetGroup)))
            {
                if (!isValidBuildTargetGroup(group)) continue;

                var defineSymbols = PlayerSettings.GetScriptingDefineSymbolsForGroup(group).Split(';').Select(d => d.Trim()).ToList();
                bool changed = false;

                foreach (string symbol in symbols)
                {
                    if (!defineSymbols.Contains(symbol))
                    {
                        defineSymbols.Add(symbol);
                        changed = true;
                    }
                }

                if (changed)
                {
                    try
                    {
                        PlayerSettings.SetScriptingDefineSymbolsForGroup(group, string.Join(";", defineSymbols.ToArray()));
                    }
                    catch (System.Exception)
                    {
                        Debug.LogError("Could not add compile defines for build target group: " + group);
                        //throw;
                    }
                }
            }
        }

        protected static void removeSymbolsFromAllTargets(params string[] symbols)
        {
            foreach (BuildTargetGroup group in System.Enum.GetValues(typeof(BuildTargetGroup)))
            {
                if (!isValidBuildTargetGroup(group)) continue;

                var defineSymbols = PlayerSettings.GetScriptingDefineSymbolsForGroup(group).Split(';').Select(d => d.Trim()).ToList();
                bool changed = false;

                foreach (string symbol in symbols)
                {
                    if (defineSymbols.Contains(symbol))
                    {
                        defineSymbols.Remove(symbol);
                        changed = true;
                    }
                }

                if (changed)
                {
                    try
                    {
                        PlayerSettings.SetScriptingDefineSymbolsForGroup(group, string.Join(";", defineSymbols.ToArray()));
                    }
                    catch (System.Exception)
                    {
                        Debug.LogError("Could not remove compile defines for build target group: " + group);
                        //throw;
                    }
                }
            }
        }

        private static bool isValidBuildTargetGroup(BuildTargetGroup group)
        {
            if (group == BuildTargetGroup.Unknown || isObsolete(group))
                return false;

            /*
            #if UNITY_5_6 // Unity 5.6 bug
                        if ((int)(object)group == 27) return false;
            #endif
            */

            if (Application.unityVersion.StartsWith("5.6"))
            {
                if ((int)(object)group == 27)
                    return false;
            }

            return true;
        }

        private static bool isObsolete(System.Enum value)
        {
            int enumInt = (int)(object)value;

            if (enumInt == 4 || enumInt == 14)
                return false;

            System.Reflection.FieldInfo field = value.GetType().GetField(value.ToString());
            System.ObsoleteAttribute[] attributes = (System.ObsoleteAttribute[])field.GetCustomAttributes(typeof(System.ObsoleteAttribute), false);
            return attributes.Length > 0;
        }

        //TODO remove in a later version
        protected static void setCompileDefines(string[] symbols)
        {
            addSymbolsToAllTargets(symbols);
        }
    }
}
// © 2018-2019 crosstales LLC (https://www.crosstales.com)