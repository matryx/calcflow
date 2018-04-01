#if UNITY_IOS
using UnityEngine;
using UnityEditor;
using UnityEditor.Callbacks;
using MixpanelSDK.UnityEditor.iOS.Xcode;
using System;
using System.Diagnostics;
using System.Collections;
using System.IO;

public class MixpanelPostprocessScript : MonoBehaviour
{
    [PostProcessBuild]
    public static void OnPostprocessBuild(BuildTarget target, string buildPath)
    {
        UnityEngine.Debug.Log("******** START Mixpanel iOS Postprocess Script ********");

        // Find the xcodeproj based on the build path
        string projectPath = PBXProject.GetPBXProjectPath(buildPath);

        // Load and parse the xcodeproj
        PBXProject project = new PBXProject();
        project.ReadFromFile(projectPath);

        // Find the default (non-test) target
        string targetGuid = project.TargetGuidByName(PBXProject.GetUnityTargetName());

        // Perform our customizations to their xcodeproj
        AddLinkerFlags(project, targetGuid);
        AddFrameworks(project, targetGuid);
        // Remove OSX bundle to work around Unity 4.X bug that incorrectly imports the
        // OSX bundle in iOS projects
        RemoveMacBundle(project);

        project.WriteToFile (projectPath);

        UnityEngine.Debug.Log("******** END Mixpanel iOS Postprocess Script ********");
    }

    private static void AddLinkerFlags(PBXProject project, string targetGuid)
    {
        project.SetBuildProperty(targetGuid, "OTHER_LD_FLAGS", "$(inherited) -lc++");
    }

    private static void AddFrameworks(PBXProject project, string targetGuid)
    {
        project.AddFrameworkToProject(targetGuid, "CoreTelephony.framework", false);
    }

    private static void RemoveMacBundle(PBXProject project)
    {
        string bundleGuid = project.FindFileGuidByProjectPath("Frameworks/Plugins/MixpanelSDK.bundle");
        project.RemoveFile(bundleGuid);
    }
}
#endif
