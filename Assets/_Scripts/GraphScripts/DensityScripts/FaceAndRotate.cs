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
    float movement;

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
        movement = 0f;
    }

    void Update()
    {
        objDistance = Vector3.Distance(cam.transform.position, transform.position);
        if (objDistance != camCamera.nearClipPlane * cam.transform.lossyScale.x){
            //Vector3 placeholder = transform.position + (cam.transform.forward  * camCamera.nearClipPlane);
            Vector3 placeholder = cam.transform.position + (cam.transform.forward  * camCamera.nearClipPlane * cam.transform.lossyScale.x);
            if (bounds.bounds.Contains(placeholder)){
                transform.position = placeholder;
                movement = Vector3.Distance(initialPos, transform.localPosition)/(bounds.radius*bounds.transform.localScale.x*2f);
            }
            else{
                transform.position = (bounds.transform.position) - (transform.forward * bounds.radius * bounds.transform.lossyScale.x);
                movement = 0f;
            }
        }
        transform.rotation = Quaternion.LookRotation(cam.transform.position - transform.position) * Quaternion.Euler(0, 180, 0);
        relative = Quaternion.Inverse(transform.localRotation) * initialRot;
        Matrix4x4 p = Matrix4x4.TRS(transform.localPosition, relative, new Vector3(1,1,1));
        Matrix4x4 r = Matrix4x4.TRS(transform.position, relative, new Vector3(1,1,1));
        mat.SetMatrix("_DeltaPos", p);
        mat.SetMatrix("_DeltaRot", r);
        mat.SetFloat("_Movement", movement);
    }
}
