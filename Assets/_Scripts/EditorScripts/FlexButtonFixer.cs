using System.Collections;
using System.Collections.Generic;
using UnityEngine;
[ExecuteInEditMode]
public class FlexButtonFixer : MonoBehaviour {

	// Use this for initialization
	void Start () {
		
	}
	
	// Update is called once per frame
	void Update () {
        FlexButtonComponent[] allObjects = UnityEngine.Object.FindObjectsOfType<FlexButtonComponent>();
        print(allObjects.Length);
        foreach (FlexButtonComponent FBC in allObjects)
        {
            GameObject g = FBC.gameObject;
            GameObject body = g.transform.Find("Body").gameObject;
            DestroyComponent<TouchRayButton>(g);
            DestroyComponent<TouchButton>(g);
            DestroyComponent<HighlightOnRaycast>(g);
            DestroyComponent<RayCastButton>(g);
            DestroyComponent<RayCastReceiver>(g);
            DestroyComponent<Collider>(g);
            DestroyComponent<BoxCollider>(g);
            VirtualButton temp = ExactlyOneComponent<VirtualButton>(g);
            ExactlyOneComponent<FlexRayCastHighlight>(g);
            Color c; 
            ColorUtility.TryParseHtmlString("#84DFFD", out c);
            FBC.hoveringColor = c;
            ColorUtility.TryParseHtmlString("#4072AB", out c);
            FBC.selectedColor = c;


            body.GetComponent<Collider>().enabled = true;
            ExactlyOneComponent<RayCastReceiver>(body);
            ExactlyOneComponent<RayCastButton>(body);
            ExactlyOneComponent<TouchButton>(body); 
            ExactlyOneComponent<TouchRayButton>(body);
            DestroyComponent<HighlightOnRaycast>(body);
            ExactlyOneComponent<RayCastButtonForwarder>(body).target = temp;



        }
        DestroyImmediate(this);
    }

    T ExactlyOneComponent<T>(GameObject target) where T : Component
    {
        T t = target.GetComponent<T>();

        if (t == null)
        {
            t = target.AddComponent<T>();
        }

        return t;
    }

    void DestroyComponent<T>(GameObject target) where T : Component
    {
        print("Removing " + typeof(T).ToString() + " from " + target.name);
        T t = target.GetComponent<T>();
        if (t != null)
        {
            DestroyImmediate(t);
        }
    }
}
