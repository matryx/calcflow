using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace Determinants
{
    public class MoveVector3 : MonoBehaviour {
        public Transform origin; //center
        public PresentPlane presentplane;

        void Update()
        {
            var mesh = GetComponentInChildren<MeshRenderer>().enabled;
            transform.localPosition = presentplane.point3.localPosition;

            //rendering boundaries
            Vector3 position = transform.position;
            if (origin.position - position != Vector3.zero)
            {
                transform.rotation = Quaternion.LookRotation(origin.position - position);
            }
            //connect line from index 1 at 0 to the transform arrow tip that has local pos pt1
            LineRenderer line = GetComponent<LineRenderer>();
            line.SetWidth(.05f, .05f);
            line.SetPosition(0, transform.position);
            line.SetPosition(1, origin.position);

            var sharedMaterial = transform.GetComponentInChildren<MeshRenderer>().sharedMaterial;
            sharedMaterial.SetInt("_planeClippingEnabled", 1);

            presentplane.CalculatePlane();

            for (int i = 0; i < 6; i++)
            {
                sharedMaterial.SetVector("_planePos" + i, presentplane.walls[i].transform.position);
                sharedMaterial.SetVector("_planeNorm" + i, presentplane.walls[i].transform.rotation * Vector3.up);
            }

        }
    }
}