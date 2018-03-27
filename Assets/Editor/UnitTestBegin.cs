using UnityEngine;
using UnityEditor;
using System.Collections;
using System.Collections.Generic;
using System;
using System.IO;

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

       // var ser = new System.Web.Script.Serialization.JavaScriptSerializer();
        //ser.DeserializeObject(GetArg(1));
        //var x = JsonUtility.FromJson<var>(json);

        StreamReader reader = new StreamReader(GetArg(1));
        string json = reader.ReadToEnd();
        loadonplay player = JsonUtility.FromJson<loadonplay>(json);
        unitTester.GetComponent<loadonplay>().scenes = player.scenes;
        unitTester.GetComponent<loadonplay>().sceneChanger = player.sceneChanger;
        unitTester.GetComponent<loadonplay>().nullTester = player.nullTester;
        unitTester.GetComponent<loadonplay>().names = player.names;
        unitTester.GetComponent<loadonplay>().assertionType = player.assertionType;
        unitTester.GetComponent<loadonplay>().sceneName = player.sceneName;

        //        public int scenes; // how many times we change scenes in the test
        //public GameObject sceneChanger;
        //public GameObject nullTester;
        //public string[][] names; // names [name of gameobject to change scene][name of gameobjects to find in that scene to make sure they are or are not null]
        //public int[] changeScene;
        //public string[][] assertionType; //type of assertion for each comparison: isnull,isnotnull,equals, or notequals
        //public string[] sceneName; //contains the scene names after each switch

    }

    static string GetArg(int x)
    {
        var args = System.Environment.GetCommandLineArgs();
        return args[x];
    }

}   