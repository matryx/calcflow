using System.Collections;
using System.Collections.Generic;
using UnityEngine;
//using Determinants;

namespace Determinants
{
    public class RotateVector2 : MonoBehaviour {
        /* 
        public GameObject Point2; //= GameObject.FindGameObjectWithTag("Point2");
        public GameObject CubeOrigin; //= GameObject.FindGameObjectWithTag("CubeOrigin");
        // Use this for initialization
        void Start () {
            float scale = Vector3.Distance(Point2.transform.position,CubeOrigin.transform.position);
            gameObject.transform.localScale = new Vector3(0.1f,scale,0.1f);
            gameObject.transform.position = CubeOrigin.transform.position + (Point2.transform.position-CubeOrigin.transform.position)/2;
            gameObject.transform.rotation = Quaternion.LookRotation(Point2.transform.position-CubeOrigin.transform.position,Vector3.up);
            //gameObject.transform.rotation = Quaternion.LookRotation(Point2.transform.position);


            
        }
        
        // Update is called once per frame
        void Update () {
            float scale = Vector3.Distance(Point2.transform.position,CubeOrigin.transform.position);
            gameObject.transform.localScale = new Vector3(0.1f,scale,0.1f);
            gameObject.transform.position = CubeOrigin.transform.position + (Point2.transform.position-CubeOrigin.transform.position)/2;
            gameObject.transform.rotation = Quaternion.LookRotation(Point2.transform.position-CubeOrigin.transform.position,Vector3.up);
            //gameObject.transform.rotation = Quaternion.LookRotation(Point2.transform.position);
            
        }
        */
        public Transform origin; //center
        public PresentPlane presentplane;

        void Update()
        {
                var mesh = GetComponentInChildren<MeshRenderer>().enabled;
                transform.localPosition = presentplane.point1.localPosition;

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

                for (int i = 0; i < 6; i++)
                {
                    sharedMaterial.SetVector("_planePos" + i, presentplane.walls[i].transform.position);
                    sharedMaterial.SetVector("_planeNorm" + i, presentplane.walls[i].transform.rotation * Vector3.up);
                }

        }
    }
}
