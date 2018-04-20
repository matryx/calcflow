using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class NormalArrow : MonoBehaviour {

	public Transform origin;
	public Transform dummyNormal;
	public MeshRenderer forwardPlane;

	void Update () {
		GetComponentInChildren<MeshRenderer>().enabled = (forwardPlane.isVisible) ? true : false;
		
		transform.localPosition = dummyNormal.localPosition.normalized;
        Vector3 position = transform.position;

        if (origin.position - position != Vector3.zero)
        {
            transform.rotation = Quaternion.LookRotation(origin.position - position);
        }

        LineRenderer line = GetComponent<LineRenderer>();
        line.SetPosition(0, transform.position);
        line.SetPosition(1, origin.position);
	}
}
