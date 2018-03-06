using System.Collections;
using System.Collections.Generic;
using UnityEngine.EventSystems;
using UnityEngine.Assertions;
using UnityEngine;

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
        GameObject dub = GameObject.Find("DoubleIntegralScene");
        RayCastButton rcButton = dub.GetComponent<RayCastButton>();
        if (rcButton != null)
        {
            rcButton.PressButton(dub);
            yield return null; yield return null;
        }
        Assert.AreEqual(UnityEngine.SceneManagement.SceneManager.GetActiveScene().name, "8 - DoubleIntegral");

        UnityEngine.SceneManagement.SceneManager.LoadScene(1);
        yield return null; yield return null;
        Assert.AreEqual(UnityEngine.SceneManagement.SceneManager.GetActiveScene().name, "7 - DoubleIntegral");

        GameObject test = GameObject.Find("Parametrized Curve");
        Assert.IsNotNull(test);


        RayCastButton test1 = test.GetComponent<RayCastButton>();
        if (test1 != null)
        {
            test1.PressButton(test);
            yield return null; yield return null;
        }
        Assert.AreEqual(UnityEngine.SceneManagement.SceneManager.GetActiveScene().name, "2 - R1-R3");
        started = 2;
    }


}
