using UnityEngine;
using System.Collections;

public class VerticalBillboard : MonoBehaviour {

    public GameObject viveCam;

	// Use this for initialization
	void Start () {
	
	}
	
	// Update is called once per frame
	void Update () {
        Vector3 lookRot = viveCam.transform.position - transform.position;
        lookRot.y = 0;
        transform.rotation = Quaternion.LookRotation(lookRot) * Quaternion.Euler(0, 180, 0);
	}
}
