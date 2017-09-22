using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Extensions;

[ExecuteInEditMode]

public class HighLightAdder : MonoBehaviour
{

    // Use this for initialization
    void Start()
    {

    }

    // Update is called once per frame
    void Update()
    {
        AddHighlight();
    }


    void AddHighlight()
    {
        GameObject[] allObjects = UnityEngine.Object.FindObjectsOfType<GameObject>();
        List<GameObject> highlightObjects = new List<GameObject>();
        foreach (GameObject go in allObjects)
        {
            var a = go.GetComponent<FlexButtonComponent>();
            var b = go.GetComponent<QuickButton>();
            if (a != null)
            {
                if (!highlightObjects.Contains(a.gameObject))
                    highlightObjects.Add(a.gameObject);
            }
            else if (b != null)
            {
                if (!highlightObjects.Contains(b.gameObject))
                    highlightObjects.Add(b.gameObject);
            }
        }

        foreach (GameObject go in highlightObjects)
        {
            var c = go.GetComponent<HighlightOnRaycast>();
            if(c==null) c = go.AddComponent<HighlightOnRaycast>();
            c.HighlightColor = new Color(143f/255f,204f/255f,204f/255f);
        }
        foreach (GameObject go in highlightObjects)
        {
            var c = go.GetComponent<TouchRayButton>();
            if (c == null) go.AddComponent<TouchRayButton>();
        }
        DestroyImmediate(this);
    }

    void RemoveHighlight()
    {
        GameObject[] allObjects = UnityEngine.Object.FindObjectsOfType<GameObject>();
        foreach (GameObject go in allObjects)
        {
            var a = go.GetComponent<HighlightOnRaycast>();
            if (a != null)
            {
                DestroyImmediate(a);
            }

        }

        DestroyImmediate(this);
    }
}
