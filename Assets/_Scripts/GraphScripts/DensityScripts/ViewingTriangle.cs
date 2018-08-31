using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ViewingTriangle : MonoBehaviour {
    
    GameObject go;
    GameObject cam;

    void Start(){
        if (FindObjectOfType<SteamVR_Camera>())
        {
            cam = FindObjectOfType<SteamVR_Camera>().gameObject;
        }
        else
        {
            cam = Camera.main.gameObject;
        }

        go = Instantiate(new GameObject(), transform);
        go.name = "vector field mesh";
        go.transform.localPosition = new Vector3(0, 0, 0);
        go.transform.localRotation = Quaternion.identity;
    }

    void Update(){
        go.transform.rotation = Quaternion.LookRotation(cam.transform.position - transform.position) * Quaternion.Euler(0, 180, 0);
	}
}
