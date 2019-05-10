using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class laserCollision : MonoBehaviour {

    GameObject raycast;
	// Use this for initialization
	void Start () {
        raycast = GameObject.Find("Controller (right)/laser");

    }
	
	// Update is called once per frame
	void Update () {
        raycast = GameObject.Find("Controller (right)/laser");
        RaycastHit hit;
        //if (Physics.Raycast(raycast, out hit))
        //{
        //    Debug.Log("raycastraycastraycastraycast: " + hit.name);
        //}
        if (Physics.Raycast(raycast.transform.position, raycast.transform.TransformDirection(Vector3.forward), out hit, Mathf.Infinity))
        {
            //Debug.DrawRay(transform.position, transform.TransformDirection(Vector3.forward) * hit.distance, Color.yellow);
            Debug.Log("raycastraycastraycastraycast: " + hit.collider);
            Debug.Log("raycastraycastraycastraycast: " + hit.rigidbody);
        }

    }
}
