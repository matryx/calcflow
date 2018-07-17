using UnityEngine;
using System.Collections;
using System.Collections.Generic;
using System;

[ExecuteInEditMode]
[RequireComponent(typeof(LineRenderer))]
public class ClippingObject : Nanome.Core.Behaviour
{
    private GameObject clippingPlane;

    private void Awake()
    {

        var lines = GetComponent<LineRenderer>(); 
        //lines.material = Resource.shaderMaterial("BoxColliderRendererLines");
        lines.useWorldSpace = true;
        lines.enabled = false;
    }

    public enum ClippingObjectType { noClipping = 0, plane };
    public ClippingObjectType currentClippingObjectType = ClippingObjectType.noClipping;

    //preview size for the planes. Shown when the object is selected.
    public float planePreviewSize = 1.5f;

    //Positions and rotations for the planes. The rotations will be converted into normals to be used by the shaders.
    public Vector3 planePosition = Vector3.zero;
    public Vector3 planeRotation = new Vector3(0, 0, 0);

    private void OnDrawGizmosSelected()
    {
        //DrawSelected();
    }

    private void DrawSelected()
    {
        var lines = GetComponent<LineRenderer>();
        lines.enabled = false;

        switch (currentClippingObjectType)
        {
            case ClippingObjectType.noClipping:
                // draw nothing
                break;
            case ClippingObjectType.plane:
                DrawPlane(transform.TransformPoint(planePosition), (planeRotation));
                break;
        }
    }

    public Color lineColor = Color.white;
    public float lineWeight = 0.001f;

    //Only used for previewing a plane. Draws diagonals and edges of a limited flat plane.
    private void DrawPlane(Vector3 position, Vector3 euler)
    {
        var forward = Quaternion.Euler(euler) * Vector3.forward;
        var left = Quaternion.Euler(euler) * Vector3.left;

        var forwardLeft = position + forward * planePreviewSize * 0.5f + left * planePreviewSize * 0.5f;
        var forwardRight = forwardLeft - left * planePreviewSize;
        var backRight = forwardRight - forward * planePreviewSize;
        var backLeft = forwardLeft - forward * planePreviewSize;

        //Gizmos.DrawLine(position, forwardLeft);
        //Gizmos.DrawLine(position, forwardRight);
        //Gizmos.DrawLine(position, backRight);
        //Gizmos.DrawLine(position, backLeft);

        //Gizmos.DrawLine(forwardLeft, forwardRight);
        //Gizmos.DrawLine(forwardRight, backRight);
        //Gizmos.DrawLine(backRight, backLeft);
        //Gizmos.DrawLine(backLeft, forwardLeft);

        var lines = GetComponent<LineRenderer>();

        var points = new List<Vector3>(32);

        points.Add(forwardLeft);
        points.Add(forwardRight);
        points.Add(backRight);
        points.Add(backLeft);
        points.Add(forwardLeft);

        lines.startColor = lineColor;
        lines.endColor = lineColor;
        lines.widthMultiplier = lineWeight;
        lines.positionCount = points.Count;
        lines.material.color = lineColor;
        lines.SetPositions(points.ToArray());
        lines.enabled = true;

    }

    List<GameObject> targets = new List<GameObject>();
    public void AddTarget(GameObject target)
    {
        targets.Add(target);
    }

    //Ideally the clipping objects do not need to be updated every frame, but we'll just keep the logic here for simplicity purposes.
    void Update()
    {
        planeRotation = transform.eulerAngles;

        DrawSelected();

        foreach (var target in targets)
        {
            foreach (var childMeshRenderer in target.GetComponentsInChildren<MeshRenderer>())
            {
                var sharedMaterial = childMeshRenderer.sharedMaterial;

                switch (currentClippingObjectType)
                {
                    case ClippingObjectType.noClipping:
                        sharedMaterial.SetInt("_planeClippingEnabled", 0);
                        break;
                    case ClippingObjectType.plane:
                        sharedMaterial.SetInt("_planeClippingEnabled", 1);
                        sharedMaterial.SetVector("_planePos", transform.TransformPoint(planePosition));
                        //plane normal vector is the rotated 'up' vector.
                        sharedMaterial.SetVector("_planeNorm", Quaternion.Euler((planeRotation)) * Vector3.up);
                        break;
                }
            }
        }
    }
}
