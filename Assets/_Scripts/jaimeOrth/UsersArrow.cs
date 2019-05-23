using System.Collections;
using System.Collections.Generic;
using UnityEngine;


namespace orthProj
{
    public class UsersArrow : MonoBehaviour
    {

        public Transform origin; //center
        private Vector3 pt1;
        public MeshRenderer forwardPlane;
        public PresentLine presentline;

        void Update()
        {
            var mesh = GetComponentInChildren<MeshRenderer>().enabled;
            pt1 = presentline.scaledPt1; 

            transform.localPosition = pt1;

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
        }
    }
}