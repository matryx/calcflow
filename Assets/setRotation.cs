using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class setRotation : MonoBehaviour
{
    // pt1 is te vector and pt2 is the rotation.
    public GameObject pt2X, pt2Y, pt2Z, vectorOfRotation, vector;
    public float xAxis, xAxisBefore, yAxis, zAxis;

    int speed = 10;
    bool makechanges = true;
    bool grabbing = false;

    // Use this for initialization
    void Start()
    {
        xAxis = 0;
        xAxisBefore = 0;
        //yAxis = 0;
        //zAxis = 0;
    }

    // Update is called once per frame
    void Update()
    {
        Vector3 vectorSpot = new Vector3(vectorOfRotation.transform.position.x, vectorOfRotation.transform.position.y, vectorOfRotation.transform.position.z);

        transform.Rotate(Vector3.up * Time.deltaTime * 100);
        changeRot();
        // transform.localRotation = Quaternion.Euler(xAxis, yAxis, zAxis);
        if(xAxis != xAxisBefore)
        {
            //Vector3 vectorSpot = new Vector3(vectorOfRotation.transform.position.x, vectorOfRotation.transform.position.y, vectorOfRotation.transform.position.z);
            //transform.RotateAround(vectorSpot, vectorSpot, xAxis - xAxisBefore);
            xAxisBefore = xAxis;
        }

        /*if(makechanges)
        {
            transform.rotation = Quaternion.Lerp(transform.rotation, Quaternion.Euler(xAxis, yAxis, zAxis), Time.time * speed);
            transform.position = Vector3.Lerp(transform.position, new Vector3(xPos, yPos, zPos), Time.time * speed);
        }*/
    }

    public void changeRot()
    {
        // Get rotation of the vector around te cartesian axes.
        xAxis = float.Parse(pt2X.GetComponent<TextMesh>().text.Replace("_", ""));
        yAxis = float.Parse(pt2Y.GetComponent<TextMesh>().text.Replace("_", ""));
        zAxis = float.Parse(pt2Z.GetComponent<TextMesh>().text.Replace("_", ""));
    }

    public void setChild()
    {
        transform.SetParent(gameObject.transform);
    }

    // UNUSED
    public void set(string name)
    {
        // Store value of input as x, y, or z of position or rotation. Then, transform.
        switch (name)
        {
            case "pt2X":
                vector.transform.Rotate(new Vector3(vectorOfRotation.transform.position.x, vectorOfRotation.transform.position.y, vectorOfRotation.transform.position.z), xAxis);
                break;
            case "pt2Y":
                transform.localRotation = Quaternion.Euler(gameObject.transform.rotation.x, yAxis, gameObject.transform.rotation.z);
                break;
            case "pt2Z":
                transform.localRotation = Quaternion.Euler(gameObject.transform.rotation.x, gameObject.transform.rotation.y, zAxis);
                break;
            default:
                break;
        }
    }
}
