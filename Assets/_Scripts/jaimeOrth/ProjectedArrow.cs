using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace orthProj
{
    public class ProjectedArrow : MonoBehaviour
    {

        public Transform origin; //center
        private Vector3 projPt1, projPt2;
        private Vector3 projectedRez;
        public PresentLine presentline;

        void Update()
        {
            //TODO: good place to make the conditional between projecting onto a line vs a plane
            var mesh = GetComponentInChildren<MeshRenderer>().enabled;

            //grab projection calculation
            projectedRez = presentline.projectedResult;
            //scaled to project
            Vector3 scaledRes = presentline.ScaledPoint(projectedRez);

            //show them the projected value

            //funcy scaling?
            //projline.localScale = new Vector3(1, 1, scaledRes.magnitude); 

            //show them the unscaled numbers
            GetComponentInChildren<TextMesh>().text = "(" + projectedRez.x + ", " + projectedRez.y + ", " + projectedRez.z + ")";
            transform.localPosition = scaledRes;
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
}