using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class testing : MonoBehaviour {

    GameObject pt1_x;

	// Use this for initialization
	void Start () {
		
	}
	
	// Update is called once per frame
	void Update () {
        pt1_x = GameObject.Find("pt1_XInputBox");
        Debug.Log(pt1_x.GetComponent<TextMesh>().text);

    }
}
