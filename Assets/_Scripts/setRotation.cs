using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class setRotation : MonoBehaviour
{
    // pt1 is te vector and pt2 is the rotation.
    public GameObject pt2X, pt2Y, pt2Z, vectorOfRotation, vector;
    public float xAxis, xAxisBefore;
    public Vector3 posBef, posCurr;

    int speed = 10;
    public bool makechanges = false;
    bool grabbing = false;

    // Use this for initialization
    void Start()
    {
        posCurr = vectorOfRotation.transform.localPosition;
        posBef = vectorOfRotation.transform.localPosition;
        xAxis = 0;
        xAxisBefore = 0;
    }

    // Update is called once per frame
    void Update()
    {
        posCurr = vectorOfRotation.transform.localPosition;
        changeRot();
        if(posCurr != posBef)
        {
            vector.transform.Rotate(posBef, -xAxis); //TAG
            vector.transform.Rotate(new Vector3(vectorOfRotation.transform.localPosition.x, vectorOfRotation.transform.localPosition.y, vectorOfRotation.transform.localPosition.z), xAxis); //TAG
            xAxisBefore = xAxis;
            posBef = posCurr;
        }
        if (xAxis != xAxisBefore)
        {
            set();
            xAxisBefore = xAxis;
        }
    }

    public void changeRot()
    {
        // Get rotation of the vector around te cartesian axes.
        //var text = pt2X.GetComponent<TextMesh>().text.Replace("_", ""); //REMOVE
        //var text = pt2X.GetComponent<TextMesh>().text.Replace("_", ""); //REMOVE
        var parsed = float.TryParse(pt2X.GetComponent<TextMesh>().text.Replace("_", ""), out xAxis);
        if(!parsed)
        {
            xAxis = 0;
        }
        xAxis = -xAxis;
        //xAxis = float.Parse(text == "" ? "0" : text); //REMOVE
        //xAxis = -(float.Parse(text == "" ? "0" : text)); //negative, to go counter-clockwise //REMOVE
    }

    public void setChild()
    {
        transform.SetParent(gameObject.transform);
    }

    // UNUSED
    public void set()
    {
        vector.transform.Rotate(new Vector3(vectorOfRotation.transform.localPosition.x, vectorOfRotation.transform.localPosition.y, vectorOfRotation.transform.localPosition.z), xAxis);// - xAxisBefore); //TAG
    }
}
