using UnityEngine;
using UnityEditor;
using System.Collections;
using System.Collections.Generic;
using System;

public  class UnitTestBegin : MonoBehaviour
{
    public static bool end = false;
    static int ind = 0;
    static UnitTestBegin instance;
    static void BeginTests()
    {
        //String[] arguments = Environment.GetCommandLineArgs();
       // instance = this;
        //-------------------Build Scenes----------------------
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
        UnityEditor.SceneManagement.EditorSceneManager.OpenScene("Assets/_Scenes/1 - IntroScene.unity");

        //-------------------Enter Play Mode----------------------
        EditorApplication.ExecuteMenuItem("Edit/Play");

        //Should be the first scene we load (scene 1)
        print(UnityEngine.SceneManagement.SceneManager.GetActiveScene().name);


        //could parse a JSON which declares different hand positions each update cycle, and tests interactions with gameobjects
        //read the JSON filenames ( unit tests ) in as arguments using the getArg function
        //test1();


        //curve.GetComponent<TouchRayButton>().PressButton(curve);

        //begin();
        GameObject unitTester = new GameObject("UnitTester");
        unitTester.AddComponent<loadonplay>();

    }

    static string GetArg(string name)
    {
        var args = System.Environment.GetCommandLineArgs();
        for (int i = 0; i < args.Length; i++)
        {
            if (args[i] == name && args.Length > i + 1)
            {
                return args[i + 1];
            }
        }
        return null;
    }

}   