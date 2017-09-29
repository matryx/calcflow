using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class TempTestScroll : MonoBehaviour {
    Scroll scroll;

	// Use this for initialization
	void Awake () {
        scroll = GetComponentInChildren<Scroll>();

        GameObject g = Instantiate(Resources.Load("Prefabs/u", typeof(GameObject))) as GameObject;
        GameObject a = Instantiate(Resources.Load("Prefabs/u", typeof(GameObject))) as GameObject;

        scroll.addObject(g.transform);
        scroll.addObject(a.transform);
	}
	
	// Update is called once per frame
	void Update () {

    }
}
