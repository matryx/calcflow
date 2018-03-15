using System.Collections;
using System.Collections.Generic;
using UnityEngine.EventSystems;
using UnityEngine.Assertions;
using UnityEngine;
#if UNITY_EDITOR
using UnityEditor;
#endif


public class loadonplay : MonoBehaviour {

    public int started = 0;
    float cd = 5.0f;
    private static loadonplay instanceRef;
    // Use this for initialization
    private void Awake()
    {
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
    void Start () {

    }
	
	// Update is called once per frame
	void Update () {
        if (started > 0)
            return;
        cd -= Time.deltaTime;
        if (cd > 0)
            return;
        
        StartCoroutine("UnitTest");
    }

    IEnumerator UnitTest()
    {
        started = 1;
        bool done = false;

        //LOAD SCENE 8
        GameObject doubleIntegral = GameObject.Find("DoubleIntegralScene");
        doubleIntegral.GetComponent<RayCastButton>().PressButton(doubleIntegral);
        yield return null; yield return null;

        //TEST 1 ------ SCENE 8 LOADED CORRECTLY
        Assert.AreEqual(UnityEngine.SceneManagement.SceneManager.GetActiveScene().name, "8 - DoubleIntegral");

        UnityEngine.SceneManagement.SceneManager.LoadScene(1);
        yield return null; yield return null;

        //TEST 2 ------- SCENE 1 LOADED CORRECTLY
        Assert.AreEqual(UnityEngine.SceneManagement.SceneManager.GetActiveScene().name, "7 - DoubleIntegral");

        GameObject parametrizedCurve = GameObject.Find("Parametrized Curve");

        //TESTS FOR CORRECT LOADING OF GAMEOBJECTS
        Assert.IsNotNull(parametrizedCurve);
        Assert.IsNull(GameObject.Find("SceneSelectMenu"),"AssertMsg: SceneSelectMenu was not null");
        Assert.IsNull(GameObject.Find("RecenterInstruction"), "AssertMsg: RecenterInstruction was not null");
        Assert.IsNull(GameObject.Find("SoundFXManager"), "AssertMsg: SoundFXManager was not null");
        Assert.IsNull(GameObject.Find("VoiceoverSwitch"), "AssertMsg: VoiceoverSwitch was not null");
        Assert.IsNull(GameObject.Find("SoundFXSwitch"), "AssertMsg: SoundFXSwitch was not null");
        Assert.IsNull(GameObject.Find("DescriptionSwitch"), "AssertMsg: DescriptionSwitch was not null");
        Assert.IsNotNull(GameObject.Find("_STATIC"), "AssertMsg: _STATIC was null");
        Assert.IsNull(GameObject.Find("pointhand_right (1)"), "AssertMsg: pointhand_right was not null");
        Assert.IsNotNull(GameObject.Find("FloorPrefab"), "AssertMsg: FloorPrefab was null");
        Assert.IsNotNull(GameObject.Find("New Env"), "AssertMsg: New Env was null");
        Assert.IsNotNull(GameObject.Find("CombinedAvatar"), "AssertMsg: CombinedAvatar was null");
        Assert.IsNotNull(GameObject.Find("Graph"), "AssertMsg: Graph was null");
        Assert.IsNotNull(GameObject.Find("GameOptions"), "AssertMsg: GameOptions was null");
        Assert.IsNotNull(GameObject.Find("Timer"), "AssertMsg: Timer was null");
        Assert.IsNotNull(GameObject.Find("[SteamVR]"), "AssertMsg: [SteamVR] was null");

        //Press parametrized curve button, switching scenes
        parametrizedCurve.GetComponent<RayCastButton>().PressButton(parametrizedCurve);
        yield return null; yield return null;

        //TEST 3 ------- SCENE 2 LOADED CORRECTLY
        Assert.AreEqual(UnityEngine.SceneManagement.SceneManager.GetActiveScene().name, "2 - R1-R3");

        started = 2;

        yield return null; yield return null;
        //ExitApplication();

        #if UNITY_EDITOR
            EditorApplication.Exit(0);
        #endif
    }

    void ExitApplication()
    {
        Application.Quit();
    }

}
