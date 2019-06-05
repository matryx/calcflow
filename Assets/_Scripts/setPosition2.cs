using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class setPosition2 : MonoBehaviour
{
    public setPosition v1;
    // pt1 is te vector and pt2 is the rotation.
    public GameObject pt1X, pt1Y, pt1Z;
    public GameObject vectorSet, rotator, component;
    public Transform origin;

    int speed = 10;
    public bool change = false;

    // Use this for initialization
    void Start()
    {
        gameObject.transform.localScale = new Vector3(1, 1, 1);
        gameObject.transform.localPosition = new Vector3(-1, 1, 1);
        transform.SetParent(vectorSet.transform, false);
    }

    // Update is called once per frame
    void Update()
    {
        if (origin.position - transform.position != Vector3.zero)
        {
            transform.rotation = Quaternion.LookRotation(origin.position - transform.position);
        }

        LineRenderer line = GetComponent<LineRenderer>();
        line.SetPosition(0, transform.position);
        line.SetPosition(1, origin.position);

        if (transform.localPosition.x != v1.xPos)
        {
            transform.localPosition += new Vector3(v1.xPos - transform.localPosition.x, 0, 0);
        }
        if (transform.localPosition.y != v1.yPos)
        {
            transform.localPosition += new Vector3(0, v1.yPos - transform.localPosition.y, 0);
        }
        if (transform.localPosition.z != v1.zPos)
        {
            transform.localPosition += new Vector3(0, 0, v1.zPos - transform.localPosition.z);
        }
    }
}