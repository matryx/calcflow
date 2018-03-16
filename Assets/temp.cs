using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class temp : MonoBehaviour {

	public float angle;
	public Transform target;
	// Use this for initialization
	void Start () {
		
	}
	
	// Update is called once per frame
	void Update () {
		Vector3 diff = transform.position - target.position;

		/* x and y move matters */
		Vector3 zzz = new Vector3(diff.x, diff.y, 0);
		angle = Quaternion.FromToRotation(Vector3.up, zzz).eulerAngles.z;

		/* x and z move matters */
		Vector3 yyy = new Vector3(diff.x, 0, diff.z);
		angle = Quaternion.FromToRotation(Vector3.forward, yyy).eulerAngles.y;

		Vector3 xxx = new Vector3(0, diff.y, diff.z);
		angle = Quaternion.FromToRotation(Vector3.right, xxx).eulerAngles.x;


		//angle = Quaternion.FromToRotation(Vector3.forward, transform.position - target.position).eulerAngles.x; 
		print(Quaternion.FromToRotation(Vector3.forward, xxx).eulerAngles);
	}
}
