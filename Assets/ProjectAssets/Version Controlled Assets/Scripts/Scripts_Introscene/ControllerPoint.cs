using UnityEngine;
using System.Collections;

public class ControllerPoint : MonoBehaviour {

    public Transform x_controll, y_controll, z_controll;

	// Use this for initialization
	void Start () {
	
	}
	
	// Update is called once per frame
	void Update () {
        transform.localPosition = new Vector3(x_controll.localPosition.x, y_controll.localPosition.y,
            z_controll.localPosition.z);
	}
}
