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
    Quaternion relativeRot;
    float movementX;
    float movementY;
    float movementZ;
    Vector3 relativeStart;

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
        movementX = 0f;
        movementY = 0f;
        movementZ = 0f;
    }

    void Update()
    {
        objDistance = Vector3.Distance(cam.transform.position, transform.position);
        // if (objDistance != camCamera.nearClipPlane * cam.transform.lossyScale.x)
        if (objDistance != camCamera.nearClipPlane + .0001f)
        {
            Vector3 placeholder = cam.transform.position + (cam.transform.forward * camCamera.nearClipPlane * cam.transform.lossyScale.x);
            if (bounds.bounds.Contains(placeholder))
            {
                transform.position = placeholder;
                if (bounds.bounds.Contains(transform.position + (cam.transform.forward * (bounds.radius * bounds.transform.lossyScale.x))))
                {
                    relativeStart = transform.position - (-1f * cam.transform.forward * ((bounds.radius * bounds.transform.lossyScale.x) - Vector3.Distance(bounds.transform.position, transform.position)));
                }
                else
                {
                    relativeStart = transform.position - (-1f * cam.transform.forward * (Vector3.Distance(bounds.transform.position, transform.position) + (bounds.radius * bounds.transform.lossyScale.x)));
                }
                // var boundsInCamSpace = transform.InverseTransformPoint(bounds.transform.position);
                // var nearestPoint = Vector3.Project(bounds.transform.position - transform.position, cam.transform.forward) + transform.position;
                // var nearestInCamSpace = transform.InverseTransformPoint(nearestPoint);
                // var offsetInCamSpace = bounds.transform.position - nearestPoint;
                // movementX = offsetInCamSpace.x;
                // movementY = offsetInCamSpace.y;
                var relCenter = cam.transform.InverseTransformPoint(bounds.transform.position) * cam.transform.lossyScale.x;
                var relPlane = cam.transform.InverseTransformPoint(transform.position) * cam.transform.lossyScale.x;
                movementX = (relCenter.x - relPlane.x)/(bounds.radius * bounds.transform.lossyScale.x * 2f);
                movementY = (relCenter.y - relPlane.y)/(bounds.radius * bounds.transform.lossyScale.x * 2f);
                movementZ = (relPlane.z - relCenter.z)/(bounds.radius * bounds.transform.lossyScale.x * 2f) + .5f;
                //movementZ = Vector3.Distance(relativeStart, transform.position) / (bounds.radius * bounds.transform.lossyScale.x * 2f);
            }
            else
            {
                transform.position = (bounds.transform.position) + (Vector3.Normalize(cam.transform.position - bounds.transform.position) * (bounds.radius * bounds.transform.lossyScale.x));
                //if (Vector3.Distance(cam.transform.position, transform.position) > (bounds.radius * bounds.transform.lossyScale.x))
                //{
                    movementX = 0f;
                    movementY = 0f;
                    movementZ = 0f;
                //}
                //else
                //{
                    //movementX = -1f * cam.transform.InverseTransformPoint(Vector3.Project(bounds.transform.position, transform.position)).x;
                    //movementY = -1f * bounds.transform.InverseTransformPoint(Vector3.Project(bounds.transform.position, transform.position)).y;
                   // movementZ = Vector3.Distance(relativeStart, transform.position) / (bounds.radius * bounds.transform.lossyScale.x * 2f);
                //}
            }
        }
        transform.rotation = Quaternion.LookRotation(cam.transform.position - transform.position) * Quaternion.Euler(0, 180, 0);
        relativeRot = Quaternion.Inverse(transform.localRotation) * initialRot;
        Matrix4x4 p = Matrix4x4.TRS(transform.localPosition, relativeRot, new Vector3(1, 1, 1));
        Matrix4x4 r = Matrix4x4.TRS(transform.position, relativeRot, new Vector3(1, 1, 1));
        mat.SetMatrix("_DeltaPos", p);
        mat.SetMatrix("_DeltaRot", r);
        mat.SetFloat("_MoveX", movementX);
        mat.SetFloat("_MoveY", movementY);
        mat.SetFloat("_MoveZ", movementZ);
    }
}
