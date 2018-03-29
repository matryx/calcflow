using UnityEngine;
using UnityEditor;
using System.Collections;
using System.Collections.Generic;
using System;
using System.IO;

public  class UnitTestBegin : MonoBehaviour
{
    static void BeginTests()
    {
        #region Build Scenes
        //Don't have to build every scene for every test, but will for the time being
        var sceneArray = new EditorBuildSettingsScene[12];
        sceneArray[0] = new EditorBuildSettingsScene("Assets/_Scenes/0 - Matryx Advertising.unity", true);
        sceneArray[1] = new EditorBuildSettingsScene("Assets/_Scenes/1 - IntroScene.unity", true);
        sceneArray[2] = new EditorBuildSettingsScene("Assets/_Scenes/2 - R1-R3.unity", true);
        sceneArray[3] = new EditorBuildSettingsScene("Assets/_Scenes/3 - FreeParametrization.unity", true);
        sceneArray[4] = new EditorBuildSettingsScene("Assets/_Scenes/4 - R3-R3.unity", true);
        sceneArray[5] = new EditorBuildSettingsScene("Assets/_Scenes/5 - VectorField.unity", true);
        sceneArray[6] = new EditorBuildSettingsScene("Assets/_Scenes/6 - VectorAdditionCross.unity", true);
        sceneArray[7] = new EditorBuildSettingsScene("Assets/_Scenes/7 - Mobius Strip.unity", true);
        sceneArray[8] = new EditorBuildSettingsScene("Assets/_Scenes/8 - DoubleIntegral.unity", true);
        sceneArray[9] = new EditorBuildSettingsScene("Assets/_Scenes/9 - TripleIntegralRegion.unity", true);
        sceneArray[10] = new EditorBuildSettingsScene("Assets/_Scenes/10 - ParametrizationAndVectorField.unity", true);
        sceneArray[11] = new EditorBuildSettingsScene("Assets/_Scenes/11 - SphericalParam.unity", true);

        BuildPipeline.BuildPlayer(sceneArray, "StandaloneWindows64", BuildTarget.StandaloneWindows64, BuildOptions.None);
        #endregion

        #region Begin Application
        UnityEditor.SceneManagement.EditorSceneManager.OpenScene("Assets/_Scenes/1 - IntroScene.unity");
        EditorApplication.ExecuteMenuItem("Edit/Play");
        #endregion

        #region Parse Input File and Create Tester
        GameObject unitTester = new GameObject("UnitTester");
        unitTester.AddComponent<loadonplay>();

        StreamReader reader = new StreamReader(GetArg(1));
        string json = reader.ReadToEnd();
        unitTester.GetComponent<loadonplay>().testData = JsonUtility.FromJson<loadonplay.data>(json);
        #endregion

    }

    static string GetArg(int x)
    {
        var args = System.Environment.GetCommandLineArgs();
        return args[x];
    }

}   