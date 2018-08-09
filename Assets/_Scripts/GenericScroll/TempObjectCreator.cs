using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class TempObjectCreator : MonoBehaviour {
    List<Transform> example;
    public Scroll scroll;
    public int numObjs;
    public Material mat;
    public bool add;

    bool prevAdd;

	void Start () {
        example = new List<Transform>();

        scroll.enabled = false;

        for (int i = 0; i < numObjs; i++)
        {
            GameObject obj = Instantiate(Resources.Load("TestObject", typeof(GameObject))) as GameObject;
            obj.SetActive(false);
            example.Add(obj.transform);
        }

        scroll.InitializeObjects(example);
        scroll.enabled = true;
    }
	
	void Update () {
		if (add && !prevAdd)
        {
            GameObject newObj = Instantiate(Resources.Load("TestObject", typeof(GameObject))) as GameObject;
            scroll.AddObject(newObj.transform);
        }

        prevAdd = add;
	}
}
