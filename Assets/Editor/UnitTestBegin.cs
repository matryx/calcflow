using UnityEngine;
using UnityEditor;
using System;

public  class UnitTestBegin : MonoBehaviour
{
    static void BeginTests()
    {
        //String[] arguments = Environment.GetCommandLineArgs();
        //UnityEngine.EditorSceneManagement.EditorSceneManager.OpenScene(3);

        //opens a specific scene, could open scene 1 and try to click on a menu item
        UnityEditor.SceneManagement.EditorSceneManager.OpenScene("Assets/_Scenes/1 - IntroScene.unity");
        //locates a button on screen, so that we can find where we want to point to
        GameObject curve = GameObject.Find("Parametrized Curve");

        //Enters play mode
        EditorApplication.ExecuteMenuItem("Edit/Play");

        Vector3 targetPos = curve.transform.position;
        GameObject rHand = GameObject.Find("RightControllerPf");
        RayCastSender sender = rHand.GetComponent<RayCastSender>();

        //set the rotation to targetPos - current rotation, or some function to get the forward vector pointing to targetPos
        rHand.transform.SetPositionAndRotation(new Vector3(10, 10, 10), Quaternion.Euler(new Vector3(0.35f, 0, 0)));
        print(rHand.transform.position);
        print(sender.TargetPoint);

        // Spoof a trigger press ---- note this probably needs to happen after the next frame so that RayCastSender has time to update()
        // OVRInput.Button.PrimaryIndexTrigger.

       //could parse a JSON which declares different hand positions each update cycle, and tests interactions with gameobjects
       //read the JSON filenames ( unit tests ) in as arguments using the getArg function
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
    
    private void Update()
    {
        print("editor updates");
    }
}   