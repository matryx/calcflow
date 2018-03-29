using System.Collections;
using System.Collections.Generic;
using UnityEngine.EventSystems;
using UnityEngine.Assertions;
using UnityEngine;
#if UNITY_EDITOR
using UnityEditor;
#endif
public class loadonplay : MonoBehaviour
{

    #region Global Variables
    private int started = 0;
    float cd = 5.0f; //starts the unit tests after a small delay
    private static loadonplay instanceRef; //used to make sure we don't create this object in every scene and that it persists through scene changes

    //Used for each unit test
    public GameObject sceneChanger;
    public GameObject nullTester;
    public data testData;
    #endregion

    #region Instance Management
    private void Awake()
    {
        //Makes sure only once instance is created, and makes sure it get destroyed when new scenes are loaded
        if (instanceRef == null)
        {
            instanceRef = this;
            DontDestroyOnLoad(gameObject);
        }
        else
        {
            DestroyImmediate(gameObject);
        }
    }
    #endregion

    #region Unit Testing
    void Update()
    {
        //Begin testing after cd seconds to make sure everything loads correctly first
        if (started > 0)
            return;
        cd -= Time.deltaTime;
        if (cd > 0)
            return;
        StartCoroutine("UnitTest");
    }

    //Main coroutine for unit testing
    IEnumerator UnitTest()
    {
        started = 1;

        for (int i = 0; i < testData.sceneInfo.changeScene.Count; i++)
        {
            print("AssertMsg: -------------Starting Test-------------");
           
            //if changeScene == 2, instantly switch to scene without pressing a button
            if (testData.sceneInfo.changeScene[i] == 2)
            {
                print("AssertMsg: Beginning Direct Scene change to scene " + int.Parse(testData.names[i].toCheck[0]));
                UnityEngine.SceneManagement.SceneManager.LoadScene(int.Parse(testData.names[i].toCheck[0]));
                yield return null; yield return null; //wait two update frames to finish loading scenes
                Assert.AreEqual(UnityEngine.SceneManagement.SceneManager.GetActiveScene().name, testData.sceneInfo.sceneName[i], "AssertMsg: Scene name was not correct. Expected: " + testData.sceneInfo.sceneName[i] + " Given: " + UnityEngine.SceneManagement.SceneManager.GetActiveScene().name);
            }
            //if changeScene ==1, press a button on a gameobject to switch the scene
            else if (testData.sceneInfo.changeScene[i] == 1)
            {
                print("AssertMsg: Beginning Scene change via button press to scene " + testData.names[i].toCheck[0]);
                sceneChanger = GameObject.Find(testData.names[i].toCheck[0]);
                sceneChanger.GetComponent<RayCastButton>().PressButton(sceneChanger);
                yield return null; yield return null; //wait two update frames to finish loading scenes
                Assert.AreEqual(UnityEngine.SceneManagement.SceneManager.GetActiveScene().name, testData.sceneInfo.sceneName[i], "AssertMsg: Scene name was not correct. Expected: " + testData.sceneInfo.sceneName[i] + " Given: " + UnityEngine.SceneManagement.SceneManager.GetActiveScene().name);
            }
            else
            {
                yield return null; yield return null; //wait two frames to ensure accuracy between tests
            }

            // ------------------- After Scene is loaded correctly -------------------
            //for each object in the scene that we want to check, makes sure they are null or non-null when they are supposed to be
            if (testData.names[i].assertType.Count > 1)
            {
                for (int j = 1; j < testData.names[i].assertType.Count; j++)
                {
                    switch (testData.names[i].assertType[j])
                    {
                        case "IsNull":
                            Assert.IsNull(GameObject.Find(testData.names[i].toCheck[j]), "AssertMsg: " + testData.names[i].toCheck[j] + " was not null in scene " + UnityEngine.SceneManagement.SceneManager.GetActiveScene().name + ", test #" + j);
                            break;
                        case "IsNotNull":
                            Assert.IsNotNull(GameObject.Find(testData.names[i].toCheck[j]), "AssertMsg: " + testData.names[i].toCheck[j] + " was null in scene " + UnityEngine.SceneManagement.SceneManager.GetActiveScene().name + ", test #" + j);
                            break;
                    }
                }
            }
        }
        print("AssertMsg: Completed all tests");
        //Checks for the editor script and exits unity completely from it, not just stopping the game.
        //This allows for testing without graphics since normally Unity would continue running in the background.
        #if UNITY_EDITOR
        EditorApplication.Exit(0);
        #endif
    }
    #endregion

    #region Data Types
    [System.Serializable]
    public class namesData
    {
        public List<string> toCheck;
        public List<string> assertType;
    }

    [System.Serializable]
    public class info
    {
        public List<int> changeScene;
        public List<string> sceneName; //contains the scene names after each switch
    }

    [System.Serializable]
    public class data
    {
        public info sceneInfo;
        public List<namesData> names;
    }
    #endregion
}