using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class FaceAndRotate : MonoBehaviour
{

    public SphereCollider bounds;
    public Material mat;
    GameObject cam;
    Camera camCamera;
    float objDistance;
    Vector3 initialPos;
    Quaternion initialRot;
    Quaternion relative;

    void Start()
    {
        if (FindObjectOfType<SteamVR_Camera>())
        {
            cam = FindObjectOfType<SteamVR_Camera>().gameObject;
        }
        else
        {
            cam = Camera.main.gameObject;
        }
        camCamera = cam.GetComponent<Camera>();
        initialPos = transform.localPosition;
        initialRot = transform.localRotation;
    }

    void Update()
    {
        if (!bounds.bounds.Contains(cam.transform.position)){
            transform.position = (bounds.transform.position) - (transform.forward * bounds.radius * bounds.transform.lossyScale.x);
            transform.rotation = Quaternion.LookRotation(cam.transform.position - transform.position) * Quaternion.Euler(0, 180, 0);
            relative = Quaternion.Inverse(transform.localRotation) * initialRot;
            Matrix4x4 m = Matrix4x4.TRS(transform.localPosition, relative, new Vector3(1,1,1));
            Matrix4x4 r = Matrix4x4.TRS(transform.position, relative, new Vector3(1,1,1));
            mat.SetMatrix("_DeltaPos", m);
            mat.SetMatrix("_DeltaRot", r);
        }
        objDistance = Vector3.Distance(cam.transform.position, transform.position);
        if (bounds.bounds.Contains(cam.transform.position)){
            if (objDistance < camCamera.nearClipPlane * cam.transform.lossyScale.x){
                 transform.position = cam.transform.position + (cam.transform.forward*(camCamera.nearClipPlane*5f));
                 transform.rotation = Quaternion.LookRotation(cam.transform.position - transform.position) * Quaternion.Euler(0, 180, 0);
            }
        }
    }
}
