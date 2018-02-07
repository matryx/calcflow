using UnityEngine;
using UnityEditor;
using System;

public  class UnitTestBegin : MonoBehaviour
{ 
    static void BeginTests()
    {
        //String[] arguments = Environment.GetCommandLineArgs();
        //UnityEngine.EditorSceneManagement.EditorSceneManager.OpenScene(3);
        UnityEditor.SceneManagement.EditorSceneManager.OpenScene("Assets/_Scenes/3 - FreeParametrization.unity");
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