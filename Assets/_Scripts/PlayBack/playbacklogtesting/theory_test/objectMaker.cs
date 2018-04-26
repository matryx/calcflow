using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class objectMaker : MonoBehaviour {

	bool starting = true;
	public	GameObject go;
	void Start () {
		
	}
	
	// Update is called once per frame
	void Update () {
		if (starting)
		{
			starting = false;
			Instantiate(go);
			Debug.Log("made something!");
		}
	}
}
