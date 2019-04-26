using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Control2DViewer : MonoBehaviour
{

    /*
    Writen by Windexglow 11-13-10.  Use it, edit it, steal it I don't care.  
    Converted to C# 27-02-13 - no credit wanted.
    Simple flycam I made, since I couldn't find any others made public.  
    Made simple to use (drag and drop, done) for regular keyboard layout  
    wasd : basic movement
    shift : Makes camera accelerate
    space : Moves camera on X and Z axis only.  So camera doesn't gain any height*/

    private float dragSpeed = 200;
    private float slowDragSpeed = 20;
    private Vector3 dragOrigin;

    float mainSpeed = 20.0f; //regular speed
    float shiftAdd = 2.0f; //multiplied by how long shift is held.  Basically running
    float camSens = 0.125f; //How sensitive it with mouse
    private Vector3 lastMouse = new Vector3(255, 255, 255); //kind of in the middle of the screen, rather than at the top (play)
    void ManageRotation(bool shiftMode)
    {
        if (Input.GetMouseButtonDown(0))
        {
            dragOrigin = Input.mousePosition;
            return;
        }

        if (!Input.GetMouseButton(0)) return;

        Vector3 pos = Camera.main.ScreenToViewportPoint(Input.mousePosition - lastMouse);
        float speed = shiftMode ? slowDragSpeed : dragSpeed;
        Vector3 move = new Vector3(0,pos.x * speed, -pos.y * speed);
        transform.eulerAngles = new Vector3(0, transform.eulerAngles.y + move.y, transform.eulerAngles.z + move.z);
        //transform.Translate(move, Space.World);
        //transform.eulerAngles += move;
        
    }
    void ManageMovement(bool shiftMode)
    {
        //Keyboard commands
        Vector3 p = GetBaseInput();
        if (shiftMode)
        {
            p = p * shiftAdd;
        }
        else
        {
            p = p * mainSpeed;
        }

        p = p * Time.deltaTime;
        transform.Translate(p);

    }

    bool GetShiftMode()
    {
        return Input.GetKey(KeyCode.LeftShift);
    }

    void Update()
    {
        bool shiftMode = GetShiftMode();
        ManageRotation(shiftMode);
        ManageMovement(shiftMode);
        lastMouse = Input.mousePosition;
    }

    private Vector3 GetBaseInput()
    { //returns the basic values, if it's 0 than it's not active.
        Vector3 p_Velocity = new Vector3();
        if (Input.GetKey(KeyCode.W))
        {
            p_Velocity += new Vector3(-1, 0, 0);
        }
        if (Input.GetKey(KeyCode.S))
        {
            p_Velocity += new Vector3(1, 0, 0);
        }
        if (Input.GetKey(KeyCode.A))
        {
            p_Velocity += new Vector3(0, 0, -1);
        }
        if (Input.GetKey(KeyCode.D))
        {
            p_Velocity += new Vector3(0, 0, 1);
        }
        if (Input.GetKey(KeyCode.Space))
        {
            p_Velocity += new Vector3(0, 1, 0);
        }
        if (Input.GetKey(KeyCode.LeftControl))
        {
            p_Velocity += new Vector3(0, -1, 0);
        }
        return p_Velocity;
    }
}