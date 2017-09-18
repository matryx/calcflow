using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using NanoVRController;

public class SaveLoadScroll : MonoBehaviour {
    SaveLoadMenu menu;
    int angle;
    int roundBy;
    JoyStickReceiver jsReceiver;

    protected void Start()
    {
        roundBy = 90;   //divides into four quadrants
        menu = GetComponent<SaveLoadMenu>();
        jsReceiver = GetComponent<JoyStickReceiver>();
        if (jsReceiver != null)
        {
            jsReceiver.JoyStickTouched += scroll;
        }
    }

    //NOTE: need to point at the board right now to register the scroll
    //      ethan will fix it later
    void scroll(VRController c, ControllerComponentArgs e)
    {
        //if (Mathf.Abs(e.x) > Mathf.Abs(e.y)) return;

        float temp = Mathf.Atan2(e.y, e.x);             //calculate angle in rad
        temp = temp * (180 / Mathf.PI);                 //rad to degree
        temp = Mathf.Round(temp / roundBy) * roundBy;   //round to nearest set degree 
        temp = (temp + 360) % 360;                      //positive
        angle = (int)temp;


        if (angle == 90)            //UP scroll moves buttons down
        {
            //menu.moveButtons(0);
        }
        else if (angle == 270)      //DOWN scroll moves buttons up
        {
            //menu.moveButtons(1);
        }
    }

    // Update is called once per frame
    void Update()
    {

    }
}
