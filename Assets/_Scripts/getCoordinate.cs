using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class getCoordinate : MonoBehaviour {

    public Transform vector;
    Vector3 position;

	// Use this for initialization
	void Start () {
        position = vector.position;
        GetComponent<TextMesh>().text = "A: " + position;
	}
	
	// Update is called once per frame
	void Update () {
        print(GetComponent<TextMesh>().text);
        position = vector.transform.localPosition;
        GetComponent<TextMesh>().text = "A: " + position;
    }
}
