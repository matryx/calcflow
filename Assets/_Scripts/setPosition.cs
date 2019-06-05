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
        }
        else if (gameObject.name == "Vector2")
        {
            maxScale = 0;
            float x = transform.localPosition.x;
            float y = transform.localPosition.y;
            float z = transform.localPosition.z;
            if (maxScale < Mathf.Abs(x))
            {
                maxScale = x;
                isX = true;
                isY = false;
                isZ = false;     
            }
            if (maxScale < Mathf.Abs(y))
            {
                maxScale = x;
                isX = false;
                isY = true;
                isZ = false;
            }
            if (maxScale < Mathf.Abs(z))
            {
                maxScale = x;
                isX = false;
                isY = false;
                isZ = true;
            }
            if (isX)
            {
                Vector3 scaledAxis = new Vector3((transform.position.x - origin.position.x) / Mathf.Abs(x) * 10 + origin.position.x, (transform.position.y - origin.position.y) / Mathf.Abs(x) * 10 + origin.position.y, (transform.position.z - origin.position.z) / Mathf.Abs(x) * 10 + origin.position.z);
                //scaledAxis = new Vector3(scaledAxis.x, scaledAxis.z, scaledAxis.y);
                line.SetPosition(0, -scaledAxis + origin.position * 2);
                line.SetPosition(1, scaledAxis);
                isX = false;
            }
            else if(isY)
            {
                Vector3 scaledAxis = new Vector3((transform.position.x - origin.position.x) / Mathf.Abs(y) * 10 + origin.position.x, (transform.position.y - origin.position.y) / Mathf.Abs(y) * 10 + origin.position.y, (transform.position.z - origin.position.z) / Mathf.Abs(y) * 10 + origin.position.z);
                //scaledAxis = new Vector3(scaledAxis.x, scaledAxis.z, scaledAxis.y);
                line.SetPosition(0, -scaledAxis + origin.position * 2);
                line.SetPosition(1, scaledAxis);
                isY = false;
            }
            else if(isZ)
            {
                Vector3 scaledAxis = new Vector3((transform.position.x - origin.position.x) / Mathf.Abs(z) * 10 + origin.position.x, (transform.position.y - origin.position.y) / Mathf.Abs(z) * 10 + origin.position.y, (transform.position.z - origin.position.z) / Mathf.Abs(z) * 10 + origin.position.z);
                //scaledAxis = new Vector3(scaledAxis.x, scaledAxis.z, scaledAxis.y);
                line.SetPosition(0, -scaledAxis + origin.position * 2);
                line.SetPosition(1, scaledAxis);
                isZ = false;
            }
        }
        set();
        /*
        if (transform.localPosition.x != xPos)
        {
            set("pt1X");
        }
        if (transform.localPosition.z != yPos)
        {
            set("pt1Y");
        }
        if (transform.localPosition.y != zPos)
        {
            set("pt1Z");
        }*/
    }

    public void changePos()
    {
        // Get position of the vector component.
        xPos = float.Parse(pt1X.GetComponent<TextMesh>().text.Replace("_", ""));
        if (xPos > 10)
        {
            pt1X.GetComponent<TextMesh>().text = "10";
            xPos = 10;
        }
        else if (xPos < -10)
        {
            pt1X.GetComponent<TextMesh>().text = "-10";
            xPos = -10;
        }
        yPos = float.Parse(pt1Y.GetComponent<TextMesh>().text.Replace("_", ""));
        if (yPos > 10)
        {
            pt1Y.GetComponent<TextMesh>().text = "10";
            yPos = 10;
        }
        else if (yPos < -10)
        {
            pt1Y.GetComponent<TextMesh>().text = "-10";
            yPos = -10;
        }
        zPos = float.Parse(pt1Z.GetComponent<TextMesh>().text.Replace("_", ""));
        if (zPos > 10)
        {
            pt1Z.GetComponent<TextMesh>().text = "10";
            zPos = 10;
        }
        else if (zPos < -10)
        {
            pt1Z.GetComponent<TextMesh>().text = "-10";
            zPos = -10;
        }
    }
    /*
    public void set(string name)
    {
        // Store value of input as x, y, or z of position or rotation. Then, transform.
        switch(name)
        {
            case "pt1X":
                transform.localPosition += new Vector3(xPos - transform.localPosition.x, 0, 0);
                break;
            case "pt1Y":
                transform.localPosition += new Vector3(0, yPos - transform.localPosition.y, 0);
                break;
            case "pt1Z":
                transform.localPosition += new Vector3(0, 0, zPos - transform.localPosition.z);
                break;
            default:
                break;
        }
    }*/
    public void set()
    {
        // Store value of input as x, y, or z of position or rotation. Then, transform.
        transform.localPosition += new Vector3(xPos - transform.localPosition.x, 0, 0);
        transform.localPosition += new Vector3(0, yPos - transform.localPosition.y, 0);
        transform.localPosition += new Vector3(0, 0, zPos - transform.localPosition.z);
    }
}