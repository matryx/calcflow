using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace LinearAlgebraMatrix
{
    public class CalculateColNulSpace : MonoBehaviour
    {
        public PtManager ptManager;
        private Vector3 row1, row2, row3;
        private PtCoord rawPt1, rawPt2, rawPt3;
        private float coe21, coe31, coe32;

        public int nullSpaceVectors;
        public int colSpaceVectors;

        // Use this for initialization
        void Start()
        {
            //Debug.Log("---------------------------------");
        }

        // Update is called once per frame
        void Update()
        {
            //    //Debug.Log("---------------------------------");
            //    rawPt1 = ptManager.ptSet.ptCoords["pt1"];
            //    rawPt2 = ptManager.ptSet.ptCoords["pt2"];
            //    rawPt3 = ptManager.ptSet.ptCoords["pt3"];

            //    row1 = new Vector3(rawPt1.X.Value, rawPt1.Y.Value, rawPt1.Z.Value);
            //    row2 = new Vector3(rawPt2.X.Value, rawPt2.Y.Value, rawPt2.Z.Value);
            //    row3 = new Vector3(rawPt3.X.Value, rawPt3.Y.Value, rawPt3.Z.Value);
            //    //Debug.Log("row1: " + row1);
            //    //Debug.Log("row2: " + row2);
            //    //Debug.Log("row3: " + row3);


            //    coe21 = rawPt2.X.Value / rawPt1.X.Value;
            //    coe31 = rawPt3.X.Value / rawPt1.X.Value;

            //    row2 = row2 - row1 * coe21;
            //    row3 = row3 - row1 * coe31;
            //    //Debug.Log("row2: " + row2);
            //    //Debug.Log("row3: " + row3);

            //    coe32 = rawPt3.Y.Value / row2.y;
            //    row3 = row3 - row2 * coe32;


            //    //Debug.Log(" row3: " + row3);
            //    //Debug.Log("---------------------------------");



            /////// for Matrix [1,2,3;2,4,6;0,0,1];
            /// Column Space is a plane: span{[1,2,0],[3,6,1]}
            /// Null Space is a line: span{[-2,1,0]}
            colSpaceVectors = 2;
            nullSpaceVectors = 1;



        }

    }
}
