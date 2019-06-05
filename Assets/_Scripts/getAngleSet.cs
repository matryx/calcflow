using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class getAngleSet : MonoBehaviour
{

    float angle;
    public GameObject user, aText;
    TextMesh angleTxt;
    // Use this for initialization
    void Start()
    {
        angleTxt = GetComponent<TextMesh>();
        angleTxt.text = angle + "\u00B0";
    }

    // Update is called once per frame
    void Update()
    {
        transform.LookAt(user.transform);
        angle = float.Parse(aText.GetComponent<TextMesh>().text.Replace("_", ""));
        angleTxt.text = angle + "\u00B0";
    }
}
