using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class setPosition : MonoBehaviour {

    // pt1 is te vector and pt2 is the rotation.
    public GameObject pt1X, pt1Y, pt1Z;
    public GameObject vectorSet, rotator, component;
    public float xPos, yPos, zPos;

    int speed = 10;
    bool makechanges = true;
    bool grabbing = false;

	// Use this for initialization
	void Start () {
        gameObject.transform.localScale = new Vector3(1, 1, 1);
        gameObject.transform.localPosition = new Vector3(-1, 1, 1);
        transform.SetParent(vectorSet.transform, false);
    }

    // Update is called once per frame
    void Update () {
        // print(float.Parse(pt1X.GetComponent<TextMesh>().text.Replace("_", "")));

        changePos();

        /*if(makechanges)
        {
            transform.rotation = Quaternion.Lerp(transform.rotation, Quaternion.Euler(xAxis, yAxis, zAxis), Time.time * speed);
            transform.position = Vector3.Lerp(transform.position, new Vector3(xPos, yPos, zPos), Time.time * speed);
        }*/

        // Set position of vector based on calculator.
        if (transform.localPosition.x != xPos)
        {
            set("pt1X");
        }
        if (transform.localPosition.y != yPos)
        {
            set("pt1Y");
        }
        if (transform.localPosition.z != zPos)
        {
            set("pt1Z");
        }

        // }
    }

    public void changePos()
    {
        // Get position of the vector component.
        xPos = float.Parse(pt1X.GetComponent<TextMesh>().text.Replace("_", ""));
        if (xPos > 10)
        {
            pt1X.GetComponent<TextMesh>().text = "10";
        }
        else if (xPos < -10)
        {
            pt1X.GetComponent<TextMesh>().text = "-10";
        }
        yPos = float.Parse(pt1Y.GetComponent<TextMesh>().text.Replace("_", ""));
        if (yPos > 10)
        {
            pt1Y.GetComponent<TextMesh>().text = "10";
        }
        else if (yPos < -10)
        {
            pt1Y.GetComponent<TextMesh>().text = "-10";
        }
        zPos = float.Parse(pt1Z.GetComponent<TextMesh>().text.Replace("_", ""));
        if (zPos > 10)
        {
            pt1Z.GetComponent<TextMesh>().text = "10";
        }
        else if (zPos < -10)
        {
            pt1Z.GetComponent<TextMesh>().text = "-10";
        }
    }

    public void set(string name)
    {
        // Store value of input as x, y, or z of position or rotation. Then, transform.
        switch(name)
        {
            case "pt1X":
                transform.localPosition += new Vector3(xPos - transform.localPosition.x, 0, 0);
                if(gameObject.name == "Vector2")
                {
                    rotator.transform.LookAt(component.transform);
                    transform.Rotate(Vector3.right * 90);
                }
                break;
            case "pt1Y":
                transform.localPosition += new Vector3(0, yPos - transform.localPosition.y, 0);
                if (gameObject.name == "Vector2")
                {
                    rotator.transform.LookAt(component.transform);
                    transform.Rotate(Vector3.right * 90);
                }
                break;
            case "pt1Z":
                transform.localPosition += new Vector3(0, 0, zPos - transform.localPosition.z);
                if (gameObject.name == "Vector2")
                {
                    rotator.transform.LookAt(component.transform);
                    transform.Rotate(Vector3.right * 90);
                }
                break;
            default:
                break;
        }
    }
}
