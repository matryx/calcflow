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
        //var text = aText.GetComponent<TextMesh>().text.Replace("_", "");
        //angle = float.Parse(text == "" ? "0" : text);
        var parsed = float.TryParse(aText.GetComponent<TextMesh>().text.Replace("_", ""), out angle);
        if(!parsed)
        {
            angle = 0;
        }
        angleTxt.text = angle + "\u00B0";
    }
}
