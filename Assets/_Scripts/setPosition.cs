using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class setPosition : MonoBehaviour {

    public setPosition2 v3;
    // pt1 is te vector and pt2 is the rotation.
    public GameObject pt1X, pt1Y, pt1Z;
    public GameObject vectorSet, rotator, component;
    public float xPos, yPos, zPos;
    public Transform origin;

    float maxScale = 0;
    int speed = 10;
    public bool makechanges = false;
    bool grabbing = false;
    bool isX = false;
    bool isY = false;
    bool isZ = false;

    // Use this for initialization
    void Start () {
        gameObject.transform.localScale = new Vector3(1, 1, 1);
        gameObject.transform.localPosition = new Vector3(-1, 1, 1);
        transform.SetParent(vectorSet.transform, false);
    }

    // Update is called once per frame
    void Update () {

        changePos();

        if (origin.position - transform.position != Vector3.zero)
        {
            transform.rotation = Quaternion.LookRotation(origin.position - transform.position);
        }

        LineRenderer line = GetComponent<LineRenderer>();
        if(gameObject.name == "Vector1")
        {
            line.SetPosition(0, transform.position);
            line.SetPosition(1, origin.position);
            transform.localPosition += new Vector3(xPos - transform.localPosition.x, 0, 0);
            transform.localPosition += new Vector3(0, yPos - transform.localPosition.y, 0);
            transform.localPosition += new Vector3(0, 0, zPos - transform.localPosition.z);
        }
        else if (gameObject.name == "Vector2")
        {
            maxScale = 0;
            if(xPos == 0 && yPos == 0 && zPos == 0)
            {
                line.SetPosition(0, origin.position);
                line.SetPosition(1, origin.position);
            }
            else
            {
                if (maxScale < Mathf.Abs(xPos))
                {
                    maxScale = Mathf.Abs(xPos);
                    isX = true;
                    isY = false;
                    isZ = false;
                }
                if (maxScale < Mathf.Abs(yPos))
                {
                    maxScale = Mathf.Abs(yPos);
                    isX = false;
                    isY = true;
                    isZ = false;
                }
                if (maxScale < Mathf.Abs(zPos))
                {
                    maxScale = Mathf.Abs(zPos);
                    isX = false;
                    isY = false;
                    isZ = true;
                }
                if (isX)
                {
                    Vector3 scaledAxis = new Vector3((transform.position.x - origin.position.x) / Mathf.Abs(xPos) * 10 + origin.position.x, (transform.position.y - origin.position.y) / Mathf.Abs(xPos) * 10 + origin.position.y, (transform.position.z - origin.position.z) / Mathf.Abs(xPos) * 10 + origin.position.z);
                    //scaledAxis = new Vector3(scaledAxis.x, scaledAxis.z, scaledAxis.y);
                    //line.SetPosition(0, -scaledAxis + origin.position * 2);
                    line.SetPosition(0, origin.position);
                    line.SetPosition(1, scaledAxis);
                    isX = false;
                }
                else if (isY)
                {
                    Vector3 scaledAxis = new Vector3((transform.position.x - origin.position.x) / Mathf.Abs(yPos) * 10 + origin.position.x, (transform.position.y - origin.position.y) / Mathf.Abs(yPos) * 10 + origin.position.y, (transform.position.z - origin.position.z) / Mathf.Abs(yPos) * 10 + origin.position.z);
                    //scaledAxis = new Vector3(scaledAxis.x, scaledAxis.z, scaledAxis.y);
                    //line.SetPosition(0, -scaledAxis + origin.position * 2);
                    line.SetPosition(0, origin.position);
                    line.SetPosition(1, scaledAxis);
                    isY = false;
                }
                else if (isZ)
                {
                    Vector3 scaledAxis = new Vector3((transform.position.x - origin.position.x) / Mathf.Abs(zPos) * 10 + origin.position.x, (transform.position.y - origin.position.y) / Mathf.Abs(zPos) * 10 + origin.position.y, (transform.position.z - origin.position.z) / Mathf.Abs(zPos) * 10 + origin.position.z);
                    //scaledAxis = new Vector3(scaledAxis.x, scaledAxis.z, scaledAxis.y);
                    //line.SetPosition(0, -scaledAxis + origin.position * 2);
                    line.SetPosition(0, origin.position);
                    line.SetPosition(1, scaledAxis);
                    isZ = false;
                }
            }
            set();
        }
    }

    public void changePos()
    {
        // Get position of the vector component.
        var parsed = float.TryParse(pt1X.GetComponent<TextMesh>().text.Replace("_", ""), out xPos);
        if (!parsed)
        {
            xPos = 0;
        }
        else if (xPos < -10 || xPos > 10)
        {
            xPos = Mathf.Clamp(xPos, -10, 10);
            pt1X.GetComponent<TextMesh>().text = xPos.ToString();
        }

        //interal values for Unity "Y" are actually "Z" due to coordinate system difference
        parsed = float.TryParse(pt1Y.GetComponent<TextMesh>().text.Replace("_", ""), out zPos); //swapped from yPos
        if (!parsed)
        {
            zPos = 0; //swapped from yPos
        }
        else if (zPos < -10 || zPos > 10) //swapped from yPos
        {
            zPos = Mathf.Clamp(zPos, -10, 10); //swapped from yPos
            pt1Y.GetComponent<TextMesh>().text = zPos.ToString(); //swapped from yPos
        }

        //interal values for Unity "Z" are actually "Y" due to coordinate system difference
        parsed = float.TryParse(pt1Z.GetComponent<TextMesh>().text.Replace("_", ""), out yPos); //swapped from zPos
        if (!parsed)
        {
            yPos = 0; //swapped from zPos
        }
        else if (yPos < -10 || yPos > 10) //swapped from zPos
        {
            yPos = Mathf.Clamp(yPos, -10, 10); //swapped from zPos
            pt1Z.GetComponent<TextMesh>().text = yPos.ToString(); //swapped from zPos
        }
    }

    public void set()
    {
        // Store value of input as x, y, or z of position or rotation. Then, transform.
        transform.localPosition += new Vector3(xPos - transform.localPosition.x, 0, 0);
        transform.localPosition += new Vector3(0, yPos - transform.localPosition.y, 0);
        transform.localPosition += new Vector3(0, 0, zPos - transform.localPosition.z);
    }
}