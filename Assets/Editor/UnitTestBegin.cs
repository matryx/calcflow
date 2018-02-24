using UnityEngine;
using UnityEditor;
using System;

public  class UnitTestBegin : MonoBehaviour
{
    static int ind = 0;
    static void BeginTests()
    {
        //String[] arguments = Environment.GetCommandLineArgs();
        //UnityEngine.EditorSceneManagement.EditorSceneManager.OpenScene(3);

        //opens a specific scene, could open scene 1 and try to click on a menu item
        var sceneArray = new EditorBuildSettingsScene[2];
        sceneArray[0] = new EditorBuildSettingsScene("Assets/_Scenes/1 - IntroScene.unity", true);
        sceneArray[1] = new EditorBuildSettingsScene("Assets/_Scenes/8 - DoubleIntegral.unity", true);
        EditorBuildSettings.scenes = sceneArray;
        UnityEditor.SceneManagement.EditorSceneManager.OpenScene("Assets/_Scenes/1 - IntroScene.unity");
        //locates a button on screen, so that we can find where we want to point to

        //Enters play mode
        EditorApplication.ExecuteMenuItem("Edit/Play");
        print(UnityEngine.SceneManagement.SceneManager.GetActiveScene().name);

        // Spoof a trigger press ---- note this probably needs to happen after the next frame so that RayCastSender has time to update()
        // OVRInput.Button.PrimaryIndexTrigger.

        //could parse a JSON which declares different hand positions each update cycle, and tests interactions with gameobjects
        //read the JSON filenames ( unit tests ) in as arguments using the getArg function
        //test1();


        //curve.GetComponent<TouchRayButton>().PressButton(curve);
        print(UnityEngine.SceneManagement.SceneManager.GetActiveScene().name);
        

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
    static private void Start()
    {
        UnityEditor.SceneManagement.EditorSceneManager.OpenScene("Assets/_Scenes/11 - SphericalParam.unity");
        UnityEngine.SceneManagement.SceneManager.LoadScene("Assets/_Scenes/11 - SphericalParam.unity");

        /*
        GameObject curve = GameObject.Find("Parametrized Curve");
        Vector3 targetPos = curve.transform.position;
        GameObject rHand = GameObject.Find("RightControllerPf");
        RayCastSender sender = rHand.GetComponent<RayCastSender>();

        //set the rotation to targetPos - current rotation, or some function to get the forward vector pointing to targetPos
        //rHand.transform.SetPositionAndRotation(new Vector3(0.1323853f, 1.618963f, 0.246521f), Quaternion.Euler(new Vector3(-10.206f, 23.222f,-12.707f)));
        curve.GetComponent<TouchRayButton>().GetComponent<RayCastButton>().OnButtonEnter += curve.GetComponent<TouchRayButton>().GetComponent<RayCastButton>().PressButton;
        curve.GetComponent<TouchRayButton>().GetComponent<RayCastButton>().PressButton(curve);
        print(rHand.transform.position);
        print(sender.TargetPoint);
        print("editor updates");
        */
    }

    static void OnEnable()
    {
        UnityEditor.SceneManagement.EditorSceneManager.OpenScene("Assets/_Scenes/11 - SphericalParam.unity");
        UnityEngine.SceneManagement.SceneManager.LoadScene("Assets/_Scenes/11 - SphericalParam.unity");
        GameObject curve = GameObject.Find("Parametrized Curve");
        curve.GetComponent<TouchButton>().PressButton(curve);
        print(UnityEngine.SceneManagement.SceneManager.GetActiveScene().name);
    }
}   