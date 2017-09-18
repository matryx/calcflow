using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Analytics;
using UnityEngine.SceneManagement;


public class ScalableObjectsSize : MonoBehaviour {

    private ScalableObject[] scalableObjects;
    private string scalObjName1, scalObjName2;
    private Vector3 scale1, scale2;
	// Use this for initialization
	void Start () {
        scalableObjects = FindObjectsOfType(typeof(ScalableObject)) as ScalableObject[];
        scalObjName1 = scalableObjects[0].gameObject.name;
        scale1 = scalableObjects[0].gameObject.transform.localScale;
        if (scalableObjects.Length > 1)
        {
            scalObjName2 = scalableObjects[1].gameObject.name;
            scale2 = scalableObjects[1].gameObject.transform.localScale;
        }
    }
	
	// Update is called once per frame
	void Update () {
        if (scalableObjects[0])
            scale1 = scalableObjects[0].gameObject.transform.localScale;
        if (scalableObjects.Length > 1)
        {
            if (scalableObjects[1])
                scale2 = scalableObjects[1].gameObject.transform.localScale;
        }
    }

    void CreateCustomEvent()
    {
        if (scalableObjects.Length > 1)
        {
            Analytics.CustomEvent("Size of Scalable Objects on " + SceneManager.GetActiveScene().name, new Dictionary<string, object>
            {
                {scalObjName1, scale1},
                {scalObjName2, scale2}
            });
        } else
        {
            Analytics.CustomEvent("Size of Scalable Object on " + SceneManager.GetActiveScene().name, new Dictionary<string, object>
            {
                {scalObjName1, scale1}
            });
        }
    }
    void OnDestroy()
    {
        CreateCustomEvent();
    }

    void OnApplicationQuit()
    {
        CreateCustomEvent();
    }
}
