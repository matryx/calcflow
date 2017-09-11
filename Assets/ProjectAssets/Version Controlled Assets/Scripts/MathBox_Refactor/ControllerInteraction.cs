/*************************************
* Management of vive controller inputs
*************************************/

using UnityEngine;
using System;
using System.Collections;
using System.Collections.Generic;

public class ControllerInteraction : MonoBehaviour {

    public CoordinateSystemInteraction coordSystem;
    public StateManager app;

    MathBox_Vector leftActive, rightActive;

    //Vector3 currPos;
    //Quaternion currRot;
    Vector3 currLeftPos, currRightPos;
    float movementScale = 10.0f;

    Plotter3D activeSurface;

    private SteamVR_Controller.Device leftController
    {
        get
        {
            if (transform.Find("Controller (left)").GetComponent<SteamVR_TrackedObject>().isValid)
            {
                return SteamVR_Controller.Input((int)transform.Find("Controller (left)").GetComponent<SteamVR_TrackedObject>().index);
            }
            else
            {
                return null;
            }
        }
    }
    private SteamVR_Controller.Device rightController
    {
        get
        {
            if (transform.Find("Controller (right)").GetComponent<SteamVR_TrackedObject>().isValid)
            {
                return SteamVR_Controller.Input((int)transform.Find("Controller (right)").GetComponent<SteamVR_TrackedObject>().index);
            }
            else
            {
                return null;
            }
        }
    }

    private Transform leftTip
    {
        get
        {
            return transform.Find("Controller (left)").GetComponent<InitAttachment>().tip;
        }
    }
    private Transform rightTip
    {
        get
        {
            return transform.Find("Controller (right)").GetComponent<InitAttachment>().tip;
        }
    }

	// Use this for initialization
	void Start () {
        app.OpenLeftMenu(transform.Find("Controller (left)"), "MainPanel");
        //app.OpenRightMenu(transform.FindChild("Controller (right)"), "Panel");
	}
	
	// Update is called once per frame
	void Update () {
        if(leftController != null && rightController != null)
        {
            DualHandInput();
        }
        if(leftController != null)
        {
            ReadLeftController();
        }
        if(rightController != null)
        {
            ReadRightController();
        }
    }

    void ReadLeftController()
    {
        if (leftController.GetPressDown(SteamVR_Controller.ButtonMask.Touchpad))
        {
            //app.leftMenu.EnableSelector(leftController.GetAxis(Valve.VR.EVRButtonId.k_EButton_Axis0).x * 0.045f,
            //    leftController.GetAxis(Valve.VR.EVRButtonId.k_EButton_Axis0).y * 0.045f);
            app.SetLeftMenuPress(true);
        }
        else if (leftController.GetTouchDown(SteamVR_Controller.ButtonMask.Touchpad))
        {
            //app.leftMenu.EnableSelector();
        }
        else if (leftController.GetTouch(SteamVR_Controller.ButtonMask.Touchpad))
        {
            //app.leftMenu.UpdateSelector(leftController.GetAxis(Valve.VR.EVRButtonId.k_EButton_Axis0).x * 0.045f,
            //    leftController.GetAxis(Valve.VR.EVRButtonId.k_EButton_Axis0).y * 0.045f);
        }
        else if (leftController.GetTouchUp(SteamVR_Controller.ButtonMask.Touchpad))
        {
            //app.leftMenu.DisableSelector();
            //app.SetleftMenuPress(false);
        }
        /*
        // Toggle Coordinate System
        if(leftController.GetPressDown(SteamVR_Controller.ButtonMask.Touchpad))
        {
            int currType = (int)CoordinateSystemInteraction.coordType;
            currType++;
            currType = currType % 3;
            if (Enum.IsDefined(typeof(CoordinateSystemInteraction.CoordType), currType))
            {
                CoordinateSystemInteraction.coordType = (CoordinateSystemInteraction.CoordType)currType;
            }
        }

        // Toggle Interaction Mode
        if (leftController.GetPressDown(SteamVR_Controller.ButtonMask.ApplicationMenu))
        {
            int currMode = (int)coordSystem.interactionMode;
            currMode++;
            currMode = currMode % 5;
            if(Enum.IsDefined(typeof(CoordinateSystemInteraction.InteractionMode), currMode))
            {
                coordSystem.interactionMode = (CoordinateSystemInteraction.InteractionMode)currMode;
            }
        }
        */

        // Reposition
        /*
        if (leftController.GetPressDown(SteamVR_Controller.ButtonMask.Grip))
        {
            currPos = transform.position;
            currLeftPos = leftController.transform.pos;
        }
        else if (leftController.GetPress(SteamVR_Controller.ButtonMask.Grip))
        {
            Vector3 delta = currLeftPos - leftController.transform.pos;
            //Vector3 delta = leftController.transform.pos - currLeftPos;
            transform.position = currPos + delta;
        }
        */

        // Toggle Vector Interactions
        switch (coordSystem.interactionMode)
        {
            case CoordinateSystemInteraction.InteractionMode.Create:
                if (leftActive == null && leftController.GetTouchDown(SteamVR_Controller.ButtonMask.Trigger))
                {
                    leftActive = coordSystem.CreateVector(leftTip.position);
                }
                else if (leftActive != null && leftController.GetTouchUp(SteamVR_Controller.ButtonMask.Trigger))
                {
                    if (CoordinateSystemInteraction.coordType == CoordinateSystemInteraction.CoordType.Cartesian)
                    {
                        leftActive.CreateInCartesian();
                    }
                    else
                    {
                        leftActive.CreateInGeneral();
                    }
                    leftActive = null;
                }
                else if (leftActive != null)
                {
                    leftActive.SetPosition(leftActive.GetStart, leftTip.position);
                }
                break;
            case CoordinateSystemInteraction.InteractionMode.Modify:
                if (leftActive == null && leftController.GetTouchDown(SteamVR_Controller.ButtonMask.Trigger))
                {
                    foreach (MathBox_Vector v in coordSystem.usercreated_list)
                    {
                        if ((v.GetEnd - leftTip.position).magnitude < 0.5)
                        {
                            leftActive = v;
                            break;
                        }
                    }
                }
                else if (leftActive != null && leftController.GetTouchUp(SteamVR_Controller.ButtonMask.Trigger))
                {
                    if (CoordinateSystemInteraction.coordType == CoordinateSystemInteraction.CoordType.Cartesian)
                    {
                        leftActive.FinishEditInCartesian();
                    }
                    else
                    {
                        leftActive.FinishEditInGeneral();
                    }
                    leftActive = null;
                }
                else if (leftActive != null)
                {
                    leftActive.SetPosition(leftActive.GetStart, leftTip.position);
                }
                break;
            case CoordinateSystemInteraction.InteractionMode.Operate:
                if (coordSystem.selected_vectors.Count < 2 && leftController.GetTouchDown(SteamVR_Controller.ButtonMask.Trigger))
                {
                    foreach (MathBox_Vector v in coordSystem.usercreated_list)
                    {
                        if ((v.GetEnd - leftTip.position).magnitude < 0.5)
                        {
                            coordSystem.SelectVector(v);
                            break;
                        }
                    }
                }
                else if (coordSystem.selected_vectors.Count == 2 && leftController.GetTouchDown(SteamVR_Controller.ButtonMask.Trigger))
                {
                    if (coordSystem.opMode == CoordinateSystemInteraction.OperateMode.Addition)
                    {
                        coordSystem.AddSelected();
                    }
                    else if (coordSystem.opMode == CoordinateSystemInteraction.OperateMode.CrossProduct)
                    {
                        coordSystem.CrossSelected();
                    }
                }
                else if (leftController.GetTouchDown(SteamVR_Controller.ButtonMask.Touchpad))
                {
                    if (coordSystem.opMode == CoordinateSystemInteraction.OperateMode.Addition)
                    {
                        coordSystem.opMode = CoordinateSystemInteraction.OperateMode.CrossProduct;
                    }
                    else if (coordSystem.opMode == CoordinateSystemInteraction.OperateMode.CrossProduct)
                    {
                        coordSystem.opMode = CoordinateSystemInteraction.OperateMode.Addition;
                    }
                }
                break;
                /*
            case CoordinateSystemInteraction.InteractionMode.Function3D:
                if (leftController.GetTouchDown(SteamVR_Controller.ButtonMask.Trigger) && activeSurface == null)
                {
                    activeSurface = coordSystem.CreateSurface();
                }
                else if(leftController.GetTouchDown(SteamVR_Controller.ButtonMask.Trigger) && activeSurface != null)
                {
                    activeSurface = null;
                    coordSystem.ClearSurface();
                }
                break;
            case CoordinateSystemInteraction.InteractionMode.Clear:
                if (leftController.GetTouchDown(SteamVR_Controller.ButtonMask.Trigger))
                {
                    coordSystem.ClearScene();
                }
                break;
                */
        }
    }

    void ReadRightController()
    {
        // touchpad menu
        if (rightController.GetPressDown(SteamVR_Controller.ButtonMask.Touchpad))
        {
            //app.rightMenu.EnableSelector(rightController.GetAxis(Valve.VR.EVRButtonId.k_EButton_Axis0).x * 0.045f,
            //    rightController.GetAxis(Valve.VR.EVRButtonId.k_EButton_Axis0).y * 0.045f);
            app.SetRightMenuPress(true);
        }
        else if (rightController.GetTouchDown(SteamVR_Controller.ButtonMask.Touchpad))
        {
            //app.rightMenu.EnableSelector();
        }
        else if (rightController.GetTouch(SteamVR_Controller.ButtonMask.Touchpad))
        {
            //app.rightMenu.UpdateSelector(rightController.GetAxis(Valve.VR.EVRButtonId.k_EButton_Axis0).x * 0.045f,
            //    rightController.GetAxis(Valve.VR.EVRButtonId.k_EButton_Axis0).y * 0.045f);
        }
        else if (rightController.GetTouchUp(SteamVR_Controller.ButtonMask.Touchpad))
        {
            //app.rightMenu.DisableSelector();
            //app.SetRightMenuPress(false);
        }

        /*
        // Toggle Coordinate System
        if (rightController.GetPressDown(SteamVR_Controller.ButtonMask.Touchpad))
        {
            int currType = (int)CoordinateSystemInteraction.coordType;
            currType++;
            currType = currType % 3;
            if (Enum.IsDefined(typeof(CoordinateSystemInteraction.CoordType), currType))
            {
                CoordinateSystemInteraction.coordType = (CoordinateSystemInteraction.CoordType)currType;
            }
        }

        // Toggle Interaction Mode
        if (rightController.GetPressDown(SteamVR_Controller.ButtonMask.ApplicationMenu))
        {
            int currMode = (int)coordSystem.interactionMode;
            currMode++;
            currMode = currMode % 5;
            if (Enum.IsDefined(typeof(CoordinateSystemInteraction.InteractionMode), currMode))
            {
                coordSystem.interactionMode = (CoordinateSystemInteraction.InteractionMode)currMode;
            }
        }
        */

        // Reposition
        /*
        if (rightController.GetPressDown(SteamVR_Controller.ButtonMask.Grip))
        {
            currPos = transform.position;
            currRightPos = rightController.transform.pos;
        }
        else if (rightController.GetPress(SteamVR_Controller.ButtonMask.Grip))
        {
            Vector3 delta = currRightPos - rightController.transform.pos;
            //Vector3 delta = leftController.transform.pos - currLeftPos;
            transform.position = currPos + delta;
            currRightPos = rightController.transform.pos;
        }
        */

        // Toggle Vector Interactions
        switch (coordSystem.interactionMode)
        {
            case CoordinateSystemInteraction.InteractionMode.Create:
                if (rightActive == null && rightController.GetTouchDown(SteamVR_Controller.ButtonMask.Trigger))
                {
                    rightActive = coordSystem.CreateVector(rightTip.position);
                }
                else if (rightActive != null && rightController.GetTouchUp(SteamVR_Controller.ButtonMask.Trigger))
                {
                    if (CoordinateSystemInteraction.coordType == CoordinateSystemInteraction.CoordType.Cartesian)
                    {
                        rightActive.CreateInCartesian();
                    }                    //MathBox_Vector v = Instantiate(vectorPrefab);
                    //v.SetVectorType(MathBox_Vector.VectorType.VectorField, null, null);
                    //v.SetPosition(target - offset, target + offset);
                    //v.transform.SetParent(transform, false);
                    //vectors.Add(v);


                    else
                    {
                        rightActive.CreateInGeneral();
                    }
                    rightActive = null;
                }
                else if (rightActive != null)
                {
                    rightActive.SetPosition(rightActive.GetStart, rightTip.position);
                }
                break;
            case CoordinateSystemInteraction.InteractionMode.Modify:
                if (rightActive == null && rightController.GetTouchDown(SteamVR_Controller.ButtonMask.Trigger))
                {
                    foreach (MathBox_Vector v in coordSystem.usercreated_list)
                    {
                        if ((v.GetEnd - rightTip.position).magnitude < 0.5)
                        {
                            rightActive = v;
                            break;
                        }
                    }
                }
                else if (rightActive != null && rightController.GetTouchUp(SteamVR_Controller.ButtonMask.Trigger))
                {
                    if (CoordinateSystemInteraction.coordType == CoordinateSystemInteraction.CoordType.Cartesian)
                    {
                        rightActive.FinishEditInCartesian();
                    }
                    else
                    {
                        rightActive.FinishEditInGeneral();
                    }
                    rightActive = null;
                }
                else if (rightActive != null)
                {
                    rightActive.SetPosition(rightActive.GetStart, rightTip.position);
                }
                break;
            case CoordinateSystemInteraction.InteractionMode.Operate:
                if (coordSystem.selected_vectors.Count < 2 && rightController.GetTouchDown(SteamVR_Controller.ButtonMask.Trigger))
                {
                    foreach (MathBox_Vector v in coordSystem.usercreated_list)
                    {
                        if ((v.GetEnd - rightTip.position).magnitude < 0.5)
                        {
                            coordSystem.SelectVector(v);
                            break;
                        }
                    }
                }
                else if (coordSystem.selected_vectors.Count == 2 && rightController.GetTouchDown(SteamVR_Controller.ButtonMask.Trigger))
                {
                    if (coordSystem.opMode == CoordinateSystemInteraction.OperateMode.Addition)
                    {
                        coordSystem.AddSelected();
                    }
                    else if (coordSystem.opMode == CoordinateSystemInteraction.OperateMode.CrossProduct)
                    {
                        coordSystem.CrossSelected();
                    }
                }
                else if (rightController.GetTouchDown(SteamVR_Controller.ButtonMask.Touchpad))
                {
                    if (coordSystem.opMode == CoordinateSystemInteraction.OperateMode.Addition)
                    {
                        coordSystem.opMode = CoordinateSystemInteraction.OperateMode.CrossProduct;
                    }
                    else if (coordSystem.opMode == CoordinateSystemInteraction.OperateMode.CrossProduct)
                    {
                        coordSystem.opMode = CoordinateSystemInteraction.OperateMode.Addition;
                    }
                }
                break;
                /*
            case CoordinateSystemInteraction.InteractionMode.Function3D:
                if (rightController.GetTouchDown(SteamVR_Controller.ButtonMask.Trigger) && activeSurface == null)
                {
                    activeSurface = coordSystem.CreateSurface();
                }
                else if (rightController.GetTouchDown(SteamVR_Controller.ButtonMask.Trigger) && activeSurface != null)
                {
                    activeSurface = null;
                    coordSystem.ClearSurface();
                }
                break;
            case CoordinateSystemInteraction.InteractionMode.Clear:
                if (rightController.GetTouchDown(SteamVR_Controller.ButtonMask.Trigger))
                {
                    coordSystem.ClearScene();
                }
                break;
                */
        }
    }

    void DualHandInput()
    {
        if (leftController.GetPressDown(SteamVR_Controller.ButtonMask.Grip))
        {
            currLeftPos = leftController.transform.pos;
            currRightPos = rightController.transform.pos;
            //currPos = transform.position;
            //currRot = transform.rotation;
        }
        else if (rightController.GetPressDown(SteamVR_Controller.ButtonMask.Grip))
        {
            currRightPos = rightController.transform.pos;
            currLeftPos = leftController.transform.pos;
            //currPos = transform.position;
            //currRot = transform.rotation;
        }
        else if(leftController.GetPressDown(SteamVR_Controller.ButtonMask.Grip) && rightController.GetPressDown(SteamVR_Controller.ButtonMask.Grip))
        {
            currLeftPos = leftController.transform.pos;
            currRightPos = rightController.transform.pos;
            //currPos = transform.position;
            //currRot = transform.rotation;
        }
        else if(leftController.GetPress(SteamVR_Controller.ButtonMask.Grip) && rightController.GetPress(SteamVR_Controller.ButtonMask.Grip))
        {
            Vector3 delta = ((currRightPos + currLeftPos) - (rightController.transform.pos + leftController.transform.pos)) / 2.0f;
            //transform.position = currPos + delta * movementScale;
            transform.Translate(delta * movementScale, Space.Self);

            Vector3 from = rightController.transform.pos - leftController.transform.pos;
            from.y = 0;
            Vector3 to = currRightPos - currLeftPos;
            to.y = 0;
            float angle = Vector3.Angle(from, to);
            //Debug.Log(angle);
            if (angle < 1) angle = 0;
            Vector3 temp = Vector3.Cross(from, to);
            if (temp.y < 0) angle = -angle;
            transform.RotateAround(transform.Find("Camera (head)").position, Vector3.up, angle);

            float zoom = (currRightPos - currLeftPos).magnitude - (rightController.transform.pos - leftController.transform.pos).magnitude;
            transform.localScale += new Vector3(zoom, zoom, zoom);

            //currPos = transform.position;
            //currRot = transform.rotation;
            currLeftPos = leftController.transform.pos;
            currRightPos = rightController.transform.pos;
        }
    }
}
