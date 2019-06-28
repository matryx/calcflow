using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using MathNet.Numerics.LinearAlgebra;
using MathNet.Numerics.LinearAlgebra.Factorization;
using MathNet.Numerics.Properties;

namespace LinearAlgebraMatrix
{
    public class MatrixYFX : MonoBehaviour
    {
        public PtManager ptManager;

        public PresentPlane presentPlane_col;
        public PresentPlane presentPlane_colT;
        public GameObject PlaneExpression_col;
        public PresentPlane presentPlane_Null;
        public GameObject PlaneExpression_Null;
        public TextMesh pt3ColLabel;
        public TextMesh pt3NullLabel;

        public GameObject planeCol;
        public GameObject lineCol;
        public GameObject cubeCol;
        public GameObject pt3Col;

        public GameObject planeNull;
        public GameObject lineNull;
        public GameObject cubeNull;
        public GameObject pt3Null;

        public AxisLabelManager xLabelManagerCol;
        public AxisLabelManager yLabelManagerCol;
        public AxisLabelManager zLabelManagerCol;

        public AxisLabelManager xLabelManagerNull;
        public AxisLabelManager yLabelManagerNull;
        public AxisLabelManager zLabelManagerNull;


        //float[,] orgMat = new float[3,3];
        // float[,] redMat = new float[3, 3];
        public float[,] colMat = new float[3, 3];
        public float[,] nullMat = new float[3, 3];
        public float[,] colMatT = new float[3, 3];
        public float[,] nullMatT = new float[3, 3];
        // int pt1RowNum;
        // int pt2RowNum;
        // int pt3RowNum;
        public float[] a = new float[3];
        public float[] b = new float[3];
        public float[] c = new float[3];
        public float[,] d = new float[3,3];


        // public Vector3 row1;
        // public Vector3 row2;
        // public Vector3 row3;

        public Vector3 c1;
        public Vector3 c2;
        public Vector3 c3;

        public Vector3 n1;
        public Vector3 n2;
        public Vector3 n3;

        public Vector3 c1T;
        public Vector3 c2T;
        public Vector3 c3T;

        public Vector3 n1T;
        public Vector3 n2T;
        public Vector3 n3T;

        public Vector3 c1AA;
        public Vector3 c2AA;
        public Vector3 c3AA;

        public Vector3 S1;
        public Vector3 S2;
        public Vector3 S3;

        public Vector3 n1AA;
        public Vector3 n2AA;
        public Vector3 n3AA;

        public Vector3 c1TAA;
        public Vector3 c2TAA;
        public Vector3 c3TAA;

        public Vector3 S1T;
        public Vector3 S2T;
        public Vector3 S3T;

        public Vector3 n1TAA;
        public Vector3 n2TAA;
        public Vector3 n3TAA;

        // public static int colSpaceVectorNum = 0;
        // public static int forLine;

        Matrix<float> m;
        Matrix<float> mT;
        public static int mRank;
        public Material material1;
        public Material material2;
        
        public GameObject planeColT;
        public GameObject lineColT;
        public GameObject planeNullT;
        public GameObject lineNullT;





        //public Vector3 colSpaceVector1;
        //public Vector3 colSpaceVector2;
        //public Vector3 colSpaceVector3;
        // HashSet<int> rangeCol;

        // Use this for initialization
        void Start()
        {
            // float[,] x = {{ 1, 2, 3 },
            //      { 4, 5, 6 },
            //      { 7, 8, 9 }};
            // var M = Matrix<float>.Build;
            // m = M.DenseOfArray(x);
            // Matrix m2 = m.transpose;
   

        }

        // Update is called once per frame
        void Update()
        {
            // cubeNull.GetComponent<Renderer>().material = material2;
            // Debug.Log("dfefhoeuihfohe: "+cubeNull.GetComponent<Renderer>().material);

            // rangeCol = new HashSet<int>();
            a = PtCoordToArray(ptManager.ptSet.ptCoords["pt1"]);
            b = PtCoordToArray(ptManager.ptSet.ptCoords["pt2"]);
            c = PtCoordToArray(ptManager.ptSet.ptCoords["pt3"]);
            
            var M = Matrix<float>.Build;
            float[,] x = {{ a[0], a[1], a[2] },
                          { b[0], b[1], b[2] },
                          { c[0], c[1], c[2] }};
            m = M.DenseOfArray(x);

            float[,] xT = {{ a[0], b[0], c[0] },
                           { a[1], b[1], c[1] },
                           { a[2], b[2], c[2] }};
            mT = M.DenseOfArray(xT);
            
            mRank = m.Rank();
            // arrangeRow(a, b, c);

            // rowReduce();
            // getColumnSpaceMatrix();


            // var svd = Svd(true);
            // Debug(svd.VT.EnumerateRows(0, 3).ToArray());


            getSvd();


            
            getColumnSpace();
            getNullSpace();
            getTransColumnSpace();
            getTransNullSpace();

            if (m.Rank() == 0)
            {
                planeCol.SetActive(false);
                lineCol.SetActive(false);
                // cubeCol.SetActive(false);
                cubeCol.SetActive(true);
                cubeCol.GetComponent<Renderer>().material = material2;
                pt3Col.SetActive(true);
                pt3Col.transform.localPosition = Vector3.zero;
                pt3Col.GetComponent<Renderer>().material = material1;
                pt3ColLabel.text = "(0,0,0)";
                // Debug.Log(" pt3.transform.localPosition pt3.transform.localPosition " + pt3.transform.localPosition);
                
               
                // PlaneExpression_col.GetComponent<PresentLine>().enabled = false;
                //PlaneExpression_col.GetComponent<PresentPlane>().enabled = false;

                planeNull.SetActive(false);
                lineNull.SetActive(false);
                cubeNull.SetActive(true);
                cubeNull.GetComponent<Renderer>().material = material1;
                pt3Null.SetActive(true);
                pt3Null.transform.localPosition = Vector3.zero;
                pt3Null.GetComponent<Renderer>().material = material2;
                pt3NullLabel.text = "(0,0,0)";
                ApplyGraphAdjustment();
                // PlaneExpression_Null.GetComponent<PresentLine>().enabled = false;

                planeColT.SetActive(false);
                lineColT.SetActive(false);

                planeNullT.SetActive(false);
                lineNullT.SetActive(false);


            }
            if (m.Rank() == 1)
            {
                planeCol.SetActive(false);
                lineCol.SetActive(true);
                cubeCol.SetActive(false);
                pt3Col.SetActive(false);
                // PlaneExpression_col.GetComponent<PresentPlane>().enabled = false;
                // PlaneExpression_col.GetComponent<PresentLine>().enabled = true;

                planeNull.SetActive(true);
                lineNull.SetActive(false);
                cubeNull.SetActive(false);
                pt3Null.SetActive(false);
                // PlaneExpression_Null.GetComponent<PresentPlane>().enabled = true;
                // PlaneExpression_Null.GetComponent<PresentLine>().enabled = false;

                planeColT.SetActive(false);
                lineColT.SetActive(true);

                planeNullT.SetActive(true);
                lineNullT.SetActive(false);
                
            }
            if (m.Rank() == 2)
            {
                planeCol.SetActive(true);
                lineCol.SetActive(false);
                cubeCol.SetActive(false);
                pt3Col.SetActive(false);
               // presentPlane_col.GetPlaneDirection();
                // PlaneExpression_col.GetComponent<PresentPlane>().enabled = true;
                // PlaneExpression_col.GetComponent<PresentLine>().enabled = false;
                

                planeNull.SetActive(false);
                lineNull.SetActive(true);
                cubeNull.SetActive(false);
                pt3Null.SetActive(false);
                // PlaneExpression_Null.GetComponent<PresentPlane>().enabled = false;
                // PlaneExpression_Null.GetComponent<PresentLine>().enabled = true;

                planeColT.SetActive(true);
                lineColT.SetActive(false);
               // presentPlane_colT.GetPlaneDirection();

                planeNullT.SetActive(false);
                lineNullT.SetActive(true);
                
            }
            if (m.Rank() == 3)
            {
                planeCol.SetActive(false);
                lineCol.SetActive(false);
                cubeCol.SetActive(true);
                cubeCol.GetComponent<Renderer>().material = material1;
                pt3Col.SetActive(true);
                pt3Col.transform.localPosition = Vector3.zero;
                pt3Col.GetComponent<Renderer>().material = material2;
                pt3ColLabel.text = "(0,0,0)";
                
                // PlaneExpression_col.GetComponent<PresentLine>().enabled = false;
                //PlaneExpression_col.GetComponent<PresentPlane>().enabled = false;

                planeNull.SetActive(false);
                lineNull.SetActive(false);
                // cubeNull.SetActive(false);
                cubeNull.SetActive(true);
                cubeNull.GetComponent<Renderer>().material = material2;
                pt3Null.SetActive(true);
                pt3Null.transform.localPosition = Vector3.zero;
                pt3Null.GetComponent<Renderer>().material = material1;
                pt3NullLabel.text = "(0,0,0)";
                ApplyGraphAdjustment();
                // Debug.Log(" pt3.transform.localPosition pt3.transform.localPosition " + pt3.transform.localPosition);
                
                
                // PlaneExpression_col.GetComponent<PresentLine>().enabled = false;

                planeColT.SetActive(false);
                lineColT.SetActive(false);

                planeNullT.SetActive(false);
                lineNullT.SetActive(false);
            }
        }

        public void ApplyGraphAdjustment()
        {
            Vector3 center = Vector3.zero;
            int stepSize = 5;
            int steps = 3;

            xLabelManagerCol.Min = center.x - stepSize * steps;
            yLabelManagerCol.Min = center.y - stepSize * steps;
            zLabelManagerCol.Min = center.z - stepSize * steps;
            xLabelManagerCol.Max = center.x + stepSize * steps;
            yLabelManagerCol.Max = center.y + stepSize * steps;
            zLabelManagerCol.Max = center.z + stepSize * steps;

            xLabelManagerNull.Min = center.x - stepSize * steps;
            yLabelManagerNull.Min = center.y - stepSize * steps;
            zLabelManagerNull.Min = center.z - stepSize * steps;
            xLabelManagerNull.Max = center.x + stepSize * steps;
            yLabelManagerNull.Max = center.y + stepSize * steps;
            zLabelManagerNull.Max = center.z + stepSize * steps;

        }


        void getColumnSpace()
        {
            colMat = new float[3, 3];
            for (int i=0; i<m.Rank(); i++)
            {
                Vector<float> v = m.Range()[i];
                colMat[0, i] = v[0];
                colMat[1, i] = v[1];
                colMat[2, i] = v[2];
            }
            c1 = MatColToVector(colMat, 0);
            c2 = MatColToVector(colMat, 1);
            c3 = MatColToVector(colMat, 2);

        }
        void getNullSpace()
        {
            nullMat = new float[3, 3];
            for (int i=0; i<m.Nullity(); i++)
            {
                Vector<float> v = m.Kernel()[i];
                nullMat[0, i] = v[0];
                nullMat[1, i] = v[1];
                nullMat[2, i] = v[2];
            }
            n1 = MatColToVector(nullMat, 0);
            n2 = MatColToVector(nullMat, 1);
            n3 = MatColToVector(nullMat, 2);

        }

        void getTransColumnSpace()
        {
            colMatT = new float[3, 3];
            for (int i=0; i<mT.Rank(); i++)
            {
                Vector<float> v = mT.Range()[i];
                colMatT[0, i] = v[0];
                colMatT[1, i] = v[1];
                colMatT[2, i] = v[2];
            }
            c1T = MatColToVector(colMatT, 0);
            c2T = MatColToVector(colMatT, 1);
            c3T = MatColToVector(colMatT, 2);

        }
        void getTransNullSpace()
        {
            nullMatT = new float[3, 3];
            for (int i=0; i<mT.Nullity(); i++)
            {
                Vector<float> v = mT.Kernel()[i];
                nullMatT[0, i] = v[0];
                nullMatT[1, i] = v[1];
                nullMatT[2, i] = v[2];
            }
            n1T = MatColToVector(nullMatT, 0);
            n2T = MatColToVector(nullMatT, 1);
            n3T = MatColToVector(nullMatT, 2);

        }

        void getSvd()
        {
            // for(int i =0; i<3; i++)
            // {
            // Debug.Log(m.Range()[1][0]);
            c1AA = VectorToVector3(m.svdU()[0]);
            c2AA =  VectorToVector3(m.svdU()[1]);
            c3AA =  VectorToVector3(m.svdU()[2]);
            c1TAA =  VectorToVector3(mT.svdU()[0]);
            c2TAA =  VectorToVector3(mT.svdU()[1]);
            c3TAA =  VectorToVector3(mT.svdU()[2]);
            n1AA = VectorToVector3( m.svdVT()[0]);
            n2AA =  VectorToVector3(m.svdVT()[1]);
            n3AA =  VectorToVector3(m.svdVT()[2]);
            n1TAA =  VectorToVector3(mT.svdVT()[0]);
            n2TAA =  VectorToVector3(mT.svdVT()[1]);
            n3TAA =  VectorToVector3(mT.svdVT()[2]);

            // S1 = VectorToVector3( m.svdS()[0]);
            // S2 =  VectorToVector3(m.svdS()[1]);
            // S3 =  VectorToVector3(m.svdS()[2]);
            // S1T =  VectorToVector3(mT.svdS()[0]);
            // S2T =  VectorToVector3(mT.svdS()[1]);
            // S3T =  VectorToVector3(mT.svdS()[2]);


            // }

        }


        public float[] PtCoordToArray(PtCoord pt)
        {
            float[] array = new float[3];
            array[0] = pt.X.Value;
            array[1] = pt.Y.Value;
            array[2] = pt.Z.Value;
            return array;
        }

        public Vector3 MatRowToVector(float[,] Mat, int num)
        {
            Vector3 vector = new Vector3(Mat[num, 0], Mat[num, 1], Mat[num, 2]);
            //Vector3 v2 = new Vector3(redMat[1, 0], redMat[1, 1], redMat[1, 2]);
            //Vector3 v3 = new Vector3(redMat[2, 0], redMat[2, 1], redMat[2, 2]);
            return vector;
        }

        public Vector3 MatColToVector(float[,] Mat, int num)
        {
            Vector3 vector = new Vector3(Mat[1, num], Mat[0, num], Mat[2, num]);
            //Vector3 v2 = new Vector3(redMat[1, 0], redMat[1, 1], redMat[1, 2]);
            //Vector3 v3 = new Vector3(redMat[2, 0], redMat[2, 1], redMat[2, 2]);
            return vector;
        }

        public Vector3 VectorToVector3(Vector<float> vec)
        {
            Vector3 vector = new Vector3(vec[0], vec[1], vec[2]);
            // vector = Vector3.zero;
            //Vector3 v2 = new Vector3(redMat[1, 0], redMat[1, 1], redMat[1, 2]);
            //Vector3 v3 = new Vector3(redMat[2, 0], redMat[2, 1], redMat[2, 2]);
            return vector;
        }





        // void rowReduce()
        // {

        //     colSpaceVectorNum = 0;
        //     float coe;
        //     if (row1.x != 0)
        //     {
        //         coe = row2.x / row1.x;
        //         row2 = row2 - row1 * coe;
        //         coe = row3.x / row1.x;
        //         row3 = row3 - row1 * coe;
        //         rangeCol.Add(0);
        //         colSpaceVectorNum++;
        //         if (row2.y != 0)
        //         {
        //             coe = row3.y / row2.y;
        //             row3 = row3 - row2 * coe;
        //             rangeCol.Add(1);
        //             colSpaceVectorNum++;
        //             if (row3.z != 0)
        //             {
        //                 rangeCol.Add(2);
        //                 colSpaceVectorNum++;
        //             }
        //         }
        //         else if (row3.y != 0)
        //         {
        //             rangeCol.Add(1);
        //             colSpaceVectorNum++;
        //             if (row2.z != 0)
        //             {
        //                 rangeCol.Add(2);
        //                 colSpaceVectorNum++;
        //             }
        //         }
        //         else if (row3.y == 0)
        //         {
        //             if (row2.z != 0)
        //             {
        //                 rangeCol.Add(2);
        //                 colSpaceVectorNum++;

        //             }
        //             else if (row3.z != 0)
        //             {
        //                 rangeCol.Add(2);
        //                 colSpaceVectorNum++;
        //             }
        //         }
        //     }
        //     else if (row1.y != 0)
        //     {
        //         coe = row2.y / row1.y;
        //         row2 = row2 - row1 * coe;
        //         coe = row3.y / row1.y;
        //         row3 = row3 - row1 * coe;
        //         rangeCol.Add(1);
        //         colSpaceVectorNum++;
        //         if (row2.z != 0)
        //         {
        //             rangeCol.Add(2);
        //             colSpaceVectorNum++;

        //         }
        //         else if (row3.z != 0)
        //         {
        //             rangeCol.Add(2);
        //             colSpaceVectorNum++;
        //         }
        //     }
        //     else if (row1.z != 0)
        //     {
        //         rangeCol.Add(2);
        //         colSpaceVectorNum++;
        //     }
        //     else
        //     {
        //         colSpaceVectorNum = 0;
        //     }

        // }

        // void getColumnSpaceMatrix()
        // {
        //     //colSpaceVector1 = new Vector3(0, 0, 0);
        //     //colSpaceVector2 = new Vector3(0, 0, 0);
        //     //colSpaceVector3 = new Vector3(0, 0, 0);

        //     colMat = new float[3, 3];
            

        //     foreach (int i in rangeCol)
        //     {
        //         colMat[0, i] = a[i];
        //         colMat[1, i] = b[i];
        //         colMat[2, i] = c[i];
        //         forLine = i;
        //         //Debug.Log("HashSet:  " + i);
        //     }

        //     c1 = MatColToVector(colMat, 0);
        //     c2 = MatColToVector(colMat, 1);
        //     c3 = MatColToVector(colMat, 2);
        // }

        // void makeMatrix()
        // {
        //     for (int i = 0; i < 3; i++)
        //     {
        //         redMat[pt1RowNum, i] = a[i];
        //         redMat[pt2RowNum, i] = b[i];
        //         redMat[pt3RowNum, i] = c[i];
        //     }
        //     row1 = MatRowToVector(redMat, 0);
        //     row2 = MatRowToVector(redMat, 1);
        //     row3 = MatRowToVector(redMat, 2);
        // }


        // void arrangeRow(float[] arr1, float[] arr2, float[] arr3)
        // {
        //     int v1NonZeroCol = 5;
        //     int v2NonZeroCol = 5;
        //     int v3NonZeroCol = 5;
        //     for (int i = 2; i >= 0; i--)
        //     {
        //         if (arr1[i] != 0)
        //         {
        //             v1NonZeroCol = i;
        //             //Debug.Log("v1NonZeroCol  " + v1NonZeroCol);
        //         }
        //     }
        //     for (int i = 2; i >= 0; i--)
        //     {
        //         if (arr2[i] != 0)
        //         {
        //             v2NonZeroCol = i;
        //         }
        //     }
        //     for (int i = 2; i >= 0; i--)
        //     {
        //         if (arr3[i] != 0)
        //         {
        //             v3NonZeroCol = i;
        //         }
        //     }

        //     if (v3NonZeroCol == 5)
        //     {
        //         pt3RowNum = 2;
        //         if (v2NonZeroCol == -1 || v2NonZeroCol >= v1NonZeroCol)
        //         {
        //             pt2RowNum = 1;
        //             pt1RowNum = 0;
        //         }
        //         else
        //         {
        //             pt2RowNum = 0;
        //             pt1RowNum = 1;
        //         }
        //     }
        //     else
        //     {
        //         if (v2NonZeroCol == 5)
        //         {
        //             pt2RowNum = 2;
        //             if (v3NonZeroCol >= v1NonZeroCol)
        //             {
        //                 pt3RowNum = 1;
        //                 pt1RowNum = 0;
        //             }
        //             else
        //             {
        //                 pt3RowNum = 0;
        //                 pt1RowNum = 1;
        //             }
        //         }
        //         else
        //         {
        //             if (v1NonZeroCol == 5 || (v2NonZeroCol < v1NonZeroCol && v3NonZeroCol < v1NonZeroCol))
        //             {
        //                 pt1RowNum = 2;
        //                 if (v3NonZeroCol >= v2NonZeroCol)
        //                 {
        //                     pt3RowNum = 1;
        //                     pt2RowNum = 0;
        //                 }
        //                 else
        //                 {
        //                     pt3RowNum = 0;
        //                     pt2RowNum = 1;
        //                 }
        //             }
        //             else if (v2NonZeroCol >= v1NonZeroCol && v3NonZeroCol >= v1NonZeroCol)
        //             {
        //                 pt1RowNum = 0;
        //                 if (v3NonZeroCol >= v2NonZeroCol)
        //                 {
        //                     pt3RowNum = 2;
        //                     pt2RowNum = 1;
        //                 }
        //                 else
        //                 {
        //                     pt3RowNum = 1;
        //                     pt2RowNum = 2;
        //                 }
        //             }
        //             else if (v2NonZeroCol < v1NonZeroCol && v3NonZeroCol >= v1NonZeroCol)
        //             {
        //                 pt3RowNum = 2;
        //                 pt1RowNum = 1;
        //                 pt2RowNum = 0;
        //             }
        //             else if (v2NonZeroCol >= v1NonZeroCol && v3NonZeroCol < v1NonZeroCol)
        //             {
        //                 pt3RowNum = 0;
        //                 pt1RowNum = 1;
        //                 pt2RowNum = 2;
        //             }
        //         }

        //     }

        //     makeMatrix();

        //     //Debug.Log("arr1  " + "(" + arr1[0] + "," + arr1[1] + "," + arr1[2] + ")");
        //     //Debug.Log("arr2  " + "(" + arr2[0] + "," + arr2[1] + "," + arr2[2] + ")");
        //     //Debug.Log("arr3  " + "(" + arr3[0] + "," + arr3[1] + "," + arr3[2] + ")");

        //     //Debug.Log("pt1RowNum  " + pt1RowNum);
        //     //Debug.Log("pt2RowNum  " + pt2RowNum);
        //     //Debug.Log("pt3RowNum  " + pt3RowNum);

        //     //Debug.Log("v1NonZeroCol  " + v1NonZeroCol);
        //     //Debug.Log("v2NonZeroCol  " + v2NonZeroCol);
        //     //Debug.Log("v3NonZeroCol  " + v3NonZeroCol);

        //     //Hashtable htable = new Hashtable();
        //     //htable.Add("v1NonZeroCol", v1NonZeroCol);
        //     //htable.Add("v2NonZeroCol", v2NonZeroCol);
        //     //htable.Add("v3NonZeroCol", v3NonZeroCol);
        //     //return 
        // }

        
    }

    
}
