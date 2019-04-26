using UnityEngine;
using System.Collections;
using System.Collections.Generic;
using Valve.VR;

public struct ControllerEventArgs
{
    public bool isLeft;
    public uint controllerIndex;
    public uint controllerSignal;
    public float padX, padY;
    public SteamVR_TrackedObject controller;
}

public delegate void ControllerEventHandler(object sender, ControllerEventArgs e);

public class ControllerEventManager : MonoBehaviour
{
    public InteractionHandler scene;

    public bool isLeftGrabbing = false, isRightGrabbing = false;

    public enum GripReposition
    {
        NONE,
        HORIZONTAL_MOVE,
        SIX_DOF_MOVE,
        MOVE_ROTATE,
        MOVE_ROTATE_ZOOM
    }
    public GripReposition gripOption;

    public bool hasMenu = false;

    private uint leftIndex = OpenVR.k_unTrackedDeviceIndexInvalid;
    private VRControllerState_t leftState;
    private bool leftMenuPressed = false;
    private bool leftTriggerPressed = false;
    private bool leftPadTouched = false;
    private bool leftPadPressed = false;
    private bool leftGripped = false;

    private uint rightIndex = OpenVR.k_unTrackedDeviceIndexInvalid;
    private VRControllerState_t rightState;
    private bool rightMenuPressed = false;
    private bool rightTriggerPressed = false;
    private bool rightPadTouched = false;
    private bool rightPadPressed = false;
    private bool rightGripped = false;

    private float leftX = 0f, leftY = 0f;
    private float rightX = 0f, rightY = 0f;

    public event ControllerEventHandler LeftMenuPressed;
    public event ControllerEventHandler LeftMenuHolding;
    public event ControllerEventHandler LeftMenuUnpressed;
    public event ControllerEventHandler LeftTriggerPressed;
    public event ControllerEventHandler LeftTriggerHolding;
    public event ControllerEventHandler LeftTriggerUnpressed;
    public event ControllerEventHandler LeftPadTouched;
    public event ControllerEventHandler LeftPadTouching;
    public event ControllerEventHandler LeftPadUntouched;
    public event ControllerEventHandler LeftPadPressed;
    public event ControllerEventHandler LeftPadHolding;
    public event ControllerEventHandler LeftPadUnpressed;
    public event ControllerEventHandler LeftGripPressed;
    public event ControllerEventHandler LeftGripHolding;
    public event ControllerEventHandler LeftGripUnpressed;

    public event ControllerEventHandler LeftPadUp;
    public event ControllerEventHandler LeftPadDown;
    public event ControllerEventHandler LeftPadLeft;
    //public event ControllerEventHandler LeftPadRight;

    public event ControllerEventHandler RightMenuPressed;
    public event ControllerEventHandler RightMenuHolding;
    public event ControllerEventHandler RightMenuUnpressed;
    public event ControllerEventHandler RightTriggerPressed;
    public event ControllerEventHandler RightTriggerHolding;
    public event ControllerEventHandler RightTriggerUnpressed;
    public event ControllerEventHandler RightPadTouched;
    public event ControllerEventHandler RightPadTouching;
    public event ControllerEventHandler RightPadUntouched;
    public event ControllerEventHandler RightPadPressed;
    public event ControllerEventHandler RightPadHolding;
    public event ControllerEventHandler RightPadUnpressed;
    public event ControllerEventHandler RightGripPressed;
    public event ControllerEventHandler RightGripHolding;
    public event ControllerEventHandler RightGripUnpressed;

    public event ControllerEventHandler RightPadUp;
    public event ControllerEventHandler RightPadDown;
    public event ControllerEventHandler RightPadLeft;
    public event ControllerEventHandler RightPadRight;

    SteamVR_ControllerManager controllerManager;
    Vector3 leftPos, rightPos;

    public void ClearEvents()
    {
        LeftMenuPressed = null; LeftMenuHolding = null; LeftMenuUnpressed = null;
        LeftTriggerPressed = null; LeftTriggerHolding = null; LeftTriggerUnpressed = null;
        LeftPadTouched = null; LeftPadTouching = null; LeftPadUntouched = null;
        LeftPadPressed = null; LeftPadHolding = null; LeftPadUnpressed = null;
        LeftGripPressed = null; LeftGripHolding = null; LeftGripUnpressed = null;

        RightMenuPressed = null; RightMenuHolding = null; RightMenuUnpressed = null;
        RightTriggerPressed = null; RightTriggerHolding = null; RightTriggerUnpressed = null;
        RightPadTouched = null; RightPadTouching = null; RightPadUntouched = null;
        RightPadPressed = null; RightPadHolding = null; RightPadUnpressed = null;
        RightGripPressed = null; RightGripHolding = null; RightGripUnpressed = null;
    }

    public void ClearTriggerEvents()
    {
        LeftTriggerPressed = null; LeftTriggerHolding = null; LeftTriggerUnpressed = null;
        RightTriggerPressed = null; RightTriggerHolding = null; RightTriggerUnpressed = null;
    }

    // Use this for initialization
    void Start()
    {
        controllerManager = GetComponent<SteamVR_ControllerManager>();

        if (hasMenu)
        {
            scene.OpenLeftMenu(transform.Find("Controller (left)"), "MainPanel");
        }
        leftIndex = (uint)transform.Find("Controller (left)").GetComponent<SteamVR_TrackedObject>().index;
        rightIndex = (uint)transform.Find("Controller (right)").GetComponent<SteamVR_TrackedObject>().index;
    }

    #region left controller callbacks
    public virtual void OnLeftPadUp(ControllerEventArgs e)
    {
        if (LeftPadUp != null)
        {
            LeftPadUp(this, e);
        }
    }
    public virtual void OnLeftPadDown(ControllerEventArgs e)
    {
        if (LeftPadDown != null)
        {
            LeftPadDown(this, e);
        }
    }
    public virtual void OnLeftPadLeft(ControllerEventArgs e)
    {
        if (LeftPadLeft != null)
        {
            LeftPadLeft(this, e);
        }
    }
    public virtual void OnLeftPadRight(ControllerEventArgs e)
    {
        if (LeftPadLeft != null)
        {
            LeftPadLeft(this, e);
        }
    }

    public virtual void OnLeftMenuPressed(ControllerEventArgs e)
    {
        if (LeftMenuPressed != null)
        {
            LeftMenuPressed(this, e);
        }
    }

    public virtual void OnLeftMenuHolding(ControllerEventArgs e)
    {
        if (LeftMenuHolding != null)
        {
            LeftMenuHolding(this, e);
        }
    }

    public virtual void OnLeftMenuUnpressed(ControllerEventArgs e)
    {
        if (LeftMenuUnpressed != null)
        {
            LeftMenuUnpressed(this, e);
        }
    }

    public virtual void OnLeftTriggerPressed(ControllerEventArgs e)
    {
        if (LeftTriggerPressed != null)
        {
            LeftTriggerPressed(this, e);
        }
    }

    public virtual void OnLeftTriggerHolding(ControllerEventArgs e)
    {
        if (LeftTriggerHolding != null)
        {
            LeftTriggerHolding(this, e);
        }
    }

    public virtual void OnLeftTriggerUnpressed(ControllerEventArgs e)
    {
        if (LeftTriggerUnpressed != null)
        {
            LeftTriggerUnpressed(this, e);
        }
    }

    public virtual void OnLeftPadTouched(ControllerEventArgs e)
    {
        if (LeftPadTouched != null)
        {
            LeftPadTouched(this, e);
        }
    }

    public virtual void OnLeftPadTouching(ControllerEventArgs e)
    {
        if (LeftPadTouching != null)
        {
            LeftPadTouching(this, e);
        }
    }

    public virtual void OnLeftPadUntouched(ControllerEventArgs e)
    {
        if (LeftPadUntouched != null)
        {
            LeftPadUntouched(this, e);
        }
    }

    public virtual void OnLeftPadPressed(ControllerEventArgs e)
    {
        if (LeftPadPressed != null)
        {
            LeftPadPressed(this, e);
        }
    }

    public virtual void OnLeftPadHolding(ControllerEventArgs e)
    {
        if (LeftPadHolding != null)
        {
            LeftPadHolding(this, e);
        }
    }

    public virtual void OnLeftPadUnpressed(ControllerEventArgs e)
    {
        if (LeftPadUnpressed != null)
        {
            LeftPadUnpressed(this, e);
        }
    }

    public virtual void OnLeftGripPressed(ControllerEventArgs e)
    {
        if (LeftGripPressed != null)
        {
            LeftGripPressed(this, e);
        }
    }

    public virtual void OnLeftGripHolding(ControllerEventArgs e)
    {
        if (LeftGripHolding != null)
        {
            LeftGripHolding(this, e);
        }
    }

    public virtual void OnLeftGripUnpressed(ControllerEventArgs e)
    {
        if (LeftGripUnpressed != null)
        {
            LeftGripUnpressed(this, e);
        }
    }
    #endregion

    #region right controller callbacks
    public virtual void OnRightPadUp(ControllerEventArgs e)
    {
        if (RightPadUp != null)
        {
            RightPadUp(this, e);
        }
    }
    public virtual void OnRightPadDown(ControllerEventArgs e)
    {
        if (RightPadDown != null)
        {
            RightPadDown(this, e);
        }
    }
    public virtual void OnRightPadLeft(ControllerEventArgs e)
    {
        if (RightPadLeft != null)
        {
            RightPadLeft(this, e);
        }
    }
    public virtual void OnRightPadRight(ControllerEventArgs e)
    {
        if (RightPadRight != null)
        {
            RightPadRight(this, e);
        }
    }

    public virtual void OnRightMenuPressed(ControllerEventArgs e)
    {
        if (RightMenuPressed != null)
        {
            RightMenuPressed(this, e);
        }
    }

    public virtual void OnRightMenuHolding(ControllerEventArgs e)
    {
        if (RightMenuHolding != null)
        {
            RightMenuHolding(this, e);
        }
    }

    public virtual void OnRightMenuUnpressed(ControllerEventArgs e)
    {
        if (RightMenuUnpressed != null)
        {
            RightMenuUnpressed(this, e);
        }
    }

    public virtual void OnRightTriggerPressed(ControllerEventArgs e)
    {
        if (RightTriggerPressed != null)
        {
            RightTriggerPressed(this, e);
        }
    }

    public virtual void OnRightTriggerHolding(ControllerEventArgs e)
    {
        if (RightTriggerHolding != null)
        {
            RightTriggerHolding(this, e);
        }
    }

    public virtual void OnRightTriggerUnpressed(ControllerEventArgs e)
    {
        if (RightTriggerUnpressed != null)
        {
            RightTriggerUnpressed(this, e);
        }
    }

    public virtual void OnRightPadTouched(ControllerEventArgs e)
    {
        if (RightPadTouched != null)
        {
            RightPadTouched(this, e);
        }
    }

    public virtual void OnRightPadTouching(ControllerEventArgs e)
    {
        if (RightPadTouching != null)
        {
            RightPadTouching(this, e);
        }
    }

    public virtual void OnRightPadUntouched(ControllerEventArgs e)
    {
        if (RightPadUntouched != null)
        {
            RightPadUntouched(this, e);
        }
    }

    public virtual void OnRightPadPressed(ControllerEventArgs e)
    {
        if (RightPadPressed != null)
        {
            RightPadPressed(this, e);
        }
    }

    public virtual void OnRightPadHolding(ControllerEventArgs e)
    {
        if (RightPadHolding != null)
        {
            RightPadHolding(this, e);
        }
    }

    public virtual void OnRightPadUnpressed(ControllerEventArgs e)
    {
        if (RightPadUnpressed != null)
        {
            RightPadUnpressed(this, e);
        }
    }

    public virtual void OnRightGripPressed(ControllerEventArgs e)
    {
        if (RightGripPressed != null)
        {
            RightGripPressed(this, e);
        }
    }

    public virtual void OnRightGripHolding(ControllerEventArgs e)
    {
        if (RightGripHolding != null)
        {
            RightGripHolding(this, e);
        }
    }

    public virtual void OnRightGripUnpressed(ControllerEventArgs e)
    {
        if (RightGripUnpressed != null)
        {
            RightGripUnpressed(this, e);
        }
    }
    #endregion

    // Update is called once per frame
    void Update()
    {
        //if(leftIndex == OpenVR.k_unTrackedDeviceIndexInvalid)
        //{
        leftIndex = (uint)transform.Find("Controller (left)").GetComponent<SteamVR_TrackedObject>().index;
        //}
        //if(rightIndex == OpenVR.k_unTrackedDeviceIndexInvalid)
        //{
        rightIndex = (uint)transform.Find("Controller (right)").GetComponent<SteamVR_TrackedObject>().index;
        //}

        var system = OpenVR.System;

        if (system != null && true/*system.GetControllerState(leftIndex, ref leftState)*/)
        {

            print("WARNING: this class is depricated and broken. Use VRController");
            SteamVR_TrackedObject leftController = transform.Find("Controller (left)").GetComponent<SteamVR_TrackedObject>();

            ulong trigger = leftState.ulButtonPressed & (1UL << ((int)EVRButtonId.k_EButton_SteamVR_Trigger));
            if (trigger > 0L && !leftTriggerPressed)
            {
                leftTriggerPressed = true;
                ControllerEventArgs e;
                e.controllerIndex = leftIndex;
                e.controllerSignal = (uint)leftState.ulButtonPressed;
                e.padX = leftState.rAxis0.x;
                e.padY = leftState.rAxis0.y;
                e.controller = leftController;
                e.isLeft = true;
                OnLeftTriggerPressed(e);
            }
            else if (trigger > 0L && leftTriggerPressed)
            {
                ControllerEventArgs e;
                e.controllerIndex = leftIndex;
                e.controllerSignal = (uint)leftState.ulButtonPressed;
                e.padX = leftState.rAxis0.x;
                e.padY = leftState.rAxis0.y;
                e.controller = leftController;
                e.isLeft = true;
                OnLeftTriggerHolding(e);
            }
            else if (trigger == 0L && leftTriggerPressed)
            {
                leftTriggerPressed = false;
                ControllerEventArgs e;
                e.controllerIndex = leftIndex;
                e.controllerSignal = (uint)leftState.ulButtonPressed;
                e.padX = leftState.rAxis0.x;
                e.padY = leftState.rAxis0.y;
                e.controller = leftController;
                e.isLeft = true;
                OnLeftTriggerUnpressed(e);
            }

            ulong grip = leftState.ulButtonPressed & (1UL << ((int)EVRButtonId.k_EButton_Grip));
            if (grip > 0L && !leftGripped)
            {
                leftGripped = true;
                ControllerEventArgs e;
                e.controllerIndex = leftIndex;
                e.controllerSignal = (uint)leftState.ulButtonPressed;
                e.padX = leftState.rAxis0.x;
                e.padY = leftState.rAxis0.y;
                e.controller = leftController;
                e.isLeft = true;
                OnLeftGripPressed(e);
            }
            else if (grip > 0L && leftGripped)
            {
                ControllerEventArgs e;
                e.controllerIndex = leftIndex;
                e.controllerSignal = (uint)leftState.ulButtonPressed;
                e.padX = leftState.rAxis0.x;
                e.padY = leftState.rAxis0.y;
                e.controller = leftController;
                e.isLeft = true;
                OnLeftGripHolding(e);
            }
            else if (grip == 0L && leftGripped)
            {
                leftGripped = false;
                ControllerEventArgs e;
                e.controllerIndex = leftIndex;
                e.controllerSignal = (uint)leftState.ulButtonPressed;
                e.padX = leftState.rAxis0.x;
                e.padY = leftState.rAxis0.y;
                e.controller = leftController;
                e.isLeft = true;
                OnLeftGripUnpressed(e);
            }

            ulong pad = leftState.ulButtonPressed & (1UL << ((int)EVRButtonId.k_EButton_SteamVR_Touchpad));
            if (pad > 0L && !leftPadPressed)
            {
                leftPadPressed = true;
                ControllerEventArgs e;
                e.controllerIndex = leftIndex;
                e.controllerSignal = (uint)leftState.ulButtonPressed;
                e.padX = leftState.rAxis0.x;
                e.padY = leftState.rAxis0.y;
                e.controller = leftController;
                e.isLeft = true;
                OnLeftPadPressed(e);
            }
            else if (pad > 0L && leftPadPressed)
            {
                ControllerEventArgs e;
                e.controllerIndex = leftIndex;
                e.controllerSignal = (uint)leftState.ulButtonPressed;
                e.padX = leftState.rAxis0.x;
                e.padY = leftState.rAxis0.y;
                e.controller = leftController;
                e.isLeft = true;
                OnLeftPadHolding(e);
            }
            else if (pad == 0L && leftPadPressed)
            {
                leftPadPressed = false;
                ControllerEventArgs e;
                e.controllerIndex = leftIndex;
                e.controllerSignal = (uint)leftState.ulButtonPressed;
                e.padX = leftState.rAxis0.x;
                e.padY = leftState.rAxis0.y;
                e.controller = leftController;
                e.isLeft = true;
                OnLeftPadUnpressed(e);
            }

            pad = leftState.ulButtonTouched & (1UL << ((int)EVRButtonId.k_EButton_SteamVR_Touchpad));
            if (pad > 0L && !leftPadTouched)
            {
                leftPadTouched = true;
                ControllerEventArgs e;
                e.controllerIndex = leftIndex;
                e.controllerSignal = (uint)leftState.ulButtonPressed;
                leftX = e.padX = leftState.rAxis0.x;
                rightX = e.padY = leftState.rAxis0.y;
                e.controller = leftController;
                e.isLeft = true;
                OnLeftPadTouched(e);
            }
            else if (pad > 0L && leftPadTouched)
            {
                ControllerEventArgs e;
                e.controllerIndex = leftIndex;
                e.controllerSignal = (uint)leftState.ulButtonPressed;
                e.padX = leftState.rAxis0.x;
                e.padY = leftState.rAxis0.y;
                e.controller = leftController;
                e.isLeft = true;
                OnLeftPadTouching(e);
                float diff = Mathf.Abs(leftX - e.padX);
                if (e.padX > leftX && diff > 0.2f)
                {
                    OnLeftPadRight(e);
                }
                else if (e.padX < leftX && diff > 0.2f)
                {
                    OnLeftPadLeft(e);
                }
                diff = Mathf.Abs(leftY - e.padY);
                if (e.padY > leftY && diff > 0.2f)
                {
                    OnLeftPadUp(e);
                }
                else if (e.padY < leftY && diff > 0.2f)
                {
                    OnLeftPadDown(e);
                }
                leftX = e.padX;leftY = e.padY;
            }
            else if (pad == 0L && leftPadTouched)
            {
                leftPadTouched = false;
                ControllerEventArgs e;
                e.controllerIndex = leftIndex;
                e.controllerSignal = (uint)leftState.ulButtonPressed;
                e.padX = leftState.rAxis0.x;
                e.padY = leftState.rAxis0.y;
                e.controller = leftController;
                e.isLeft = true;
                OnLeftPadUntouched(e);
                leftX = leftY = 0f;
            }

            ulong menu = leftState.ulButtonPressed & (1UL << ((int)EVRButtonId.k_EButton_ApplicationMenu));
            if (menu > 0L && !leftMenuPressed)
            {
                leftMenuPressed = true;
                ControllerEventArgs e;
                e.controllerIndex = leftIndex;
                e.controllerSignal = (uint)leftState.ulButtonPressed;
                e.padX = leftState.rAxis0.x;
                e.padY = leftState.rAxis0.y;
                e.controller = leftController;
                e.isLeft = true;
                OnLeftMenuPressed(e);
            }
            else if (menu > 0L && leftMenuPressed)
            {
                ControllerEventArgs e;
                e.controllerIndex = leftIndex;
                e.controllerSignal = (uint)leftState.ulButtonPressed;
                e.padX = leftState.rAxis0.x;
                e.padY = leftState.rAxis0.y;
                e.controller = leftController;
                e.isLeft = true;
                OnLeftMenuHolding(e);
            }
            else if (menu == 0L && leftMenuPressed)
            {
                leftMenuPressed = false;
                ControllerEventArgs e;
                e.controllerIndex = leftIndex;
                e.controllerSignal = (uint)leftState.ulButtonPressed;
                e.padX = leftState.rAxis0.x;
                e.padY = leftState.rAxis0.y;
                e.controller = leftController;
                e.isLeft = true;
                OnLeftMenuUnpressed(e);
            }
        }
        if (system != null && true/*system.GetControllerState(rightIndex, ref rightState)*/)
        {
            print("WARNING: this class is depricated and broken. Use VRController");
            SteamVR_TrackedObject rightController = transform.Find("Controller (right)").GetComponent<SteamVR_TrackedObject>();

            ulong trigger = rightState.ulButtonPressed & (1UL << ((int)EVRButtonId.k_EButton_SteamVR_Trigger));
            if (trigger > 0L && !rightTriggerPressed)
            {
                rightTriggerPressed = true;
                ControllerEventArgs e;
                e.controllerIndex = rightIndex;
                e.controllerSignal = (uint)rightState.ulButtonPressed;
                e.padX = rightState.rAxis0.x;
                e.padY = rightState.rAxis0.y;
                e.controller = rightController;
                e.isLeft = false;
                OnRightTriggerPressed(e);
            }
            else if (trigger > 0L && rightTriggerPressed)
            {
                ControllerEventArgs e;
                e.controllerIndex = rightIndex;
                e.controllerSignal = (uint)rightState.ulButtonPressed;
                e.padX = rightState.rAxis0.x;
                e.padY = rightState.rAxis0.y;
                e.controller = rightController;
                e.isLeft = false;
                OnRightTriggerHolding(e);
            }
            else if (trigger == 0L && rightTriggerPressed)
            {
                rightTriggerPressed = false;
                ControllerEventArgs e;
                e.controllerIndex = rightIndex;
                e.controllerSignal = (uint)rightState.ulButtonPressed;
                e.padX = rightState.rAxis0.x;
                e.padY = rightState.rAxis0.y;
                e.controller = rightController;
                e.isLeft = false;
                OnRightTriggerUnpressed(e);
            }

            ulong grip = rightState.ulButtonPressed & (1UL << ((int)EVRButtonId.k_EButton_Grip));
            if (grip > 0L && !rightGripped)
            {
                rightGripped = true;
                ControllerEventArgs e;
                e.controllerIndex = rightIndex;
                e.controllerSignal = (uint)rightState.ulButtonPressed;
                e.padX = rightState.rAxis0.x;
                e.padY = rightState.rAxis0.y;
                e.controller = rightController;
                e.isLeft = false;
                OnRightGripPressed(e);
            }
            else if (grip > 0L && rightGripped)
            {
                ControllerEventArgs e;
                e.controllerIndex = rightIndex;
                e.controllerSignal = (uint)rightState.ulButtonPressed;
                e.padX = rightState.rAxis0.x;
                e.padY = rightState.rAxis0.y;
                e.controller = rightController;
                e.isLeft = false;
                OnRightGripHolding(e);
            }
            else if (grip == 0L && rightGripped)
            {
                rightGripped = false;
                ControllerEventArgs e;
                e.controllerIndex = rightIndex;
                e.controllerSignal = (uint)rightState.ulButtonPressed;
                e.padX = rightState.rAxis0.x;
                e.padY = rightState.rAxis0.y;
                e.controller = rightController;
                e.isLeft = false;
                OnRightGripUnpressed(e);
            }

            ulong pad = rightState.ulButtonPressed & (1UL << ((int)EVRButtonId.k_EButton_SteamVR_Touchpad));
            if (pad > 0L && !rightPadPressed)
            {
                rightPadPressed = true;
                ControllerEventArgs e;
                e.controllerIndex = rightIndex;
                e.controllerSignal = (uint)rightState.ulButtonPressed;
                e.padX = rightState.rAxis0.x;
                e.padY = rightState.rAxis0.y;
                e.controller = rightController;
                e.isLeft = false;
                OnRightPadPressed(e);
            }
            else if (pad > 0L && rightPadPressed)
            {
                ControllerEventArgs e;
                e.controllerIndex = rightIndex;
                e.controllerSignal = (uint)rightState.ulButtonPressed;
                e.padX = rightState.rAxis0.x;
                e.padY = rightState.rAxis0.y;
                e.controller = rightController;
                e.isLeft = false;
                OnRightPadHolding(e);
            }
            else if (pad == 0L && rightPadPressed)
            {
                rightPadPressed = false;
                ControllerEventArgs e;
                e.controllerIndex = rightIndex;
                e.controllerSignal = (uint)rightState.ulButtonPressed;
                e.padX = rightState.rAxis0.x;
                e.padY = rightState.rAxis0.y;
                e.controller = rightController;
                e.isLeft = false;
                OnRightPadUnpressed(e);
            }

            pad = rightState.ulButtonTouched & (1UL << ((int)EVRButtonId.k_EButton_SteamVR_Touchpad));
            if (pad > 0L && !rightPadTouched)
            {
                rightPadTouched = true;
                ControllerEventArgs e;
                e.controllerIndex = rightIndex;
                e.controllerSignal = (uint)rightState.ulButtonPressed;
                rightX = e.padX = rightState.rAxis0.x;
                rightY = e.padY = rightState.rAxis0.y;
                e.controller = rightController;
                e.isLeft = false;
                OnRightPadTouched(e);
            }
            else if (pad > 0L && rightPadTouched)
            {
                ControllerEventArgs e;
                e.controllerIndex = rightIndex;
                e.controllerSignal = (uint)rightState.ulButtonPressed;
                e.padX = rightState.rAxis0.x;
                e.padY = rightState.rAxis0.y;
                e.controller = rightController;
                e.isLeft = false;
                OnRightPadTouching(e);
                float diff = Mathf.Abs(e.padX - rightX);
                if(e.padX > rightX && diff > 0.2f)
                {
                    OnRightPadRight(e);
                }
                else if(e.padX < rightX && diff > 0.2f)
                {
                    OnRightPadLeft(e);
                }
                diff = Mathf.Abs(e.padY - rightY);
                if(e.padY > rightY && diff > 0.2f)
                {
                    OnRightPadUp(e);
                }
                else if(e.padY < rightY && diff > 0.2f)
                {
                    OnRightPadDown(e);
                }
                rightX = e.padX;rightY = e.padY;
            }
            else if (pad == 0L && rightPadTouched)
            {
                rightPadTouched = false;
                ControllerEventArgs e;
                e.controllerIndex = rightIndex;
                e.controllerSignal = (uint)rightState.ulButtonPressed;
                e.padX = rightState.rAxis0.x;
                e.padY = rightState.rAxis0.y;
                e.controller = rightController;
                e.isLeft = false;
                OnRightPadUntouched(e);
                rightX = rightY = 0f;
            }

            ulong menu = rightState.ulButtonPressed & (1UL << ((int)EVRButtonId.k_EButton_ApplicationMenu));
            if (menu > 0L && !rightMenuPressed)
            {
                rightMenuPressed = true;
                ControllerEventArgs e;
                e.controllerIndex = rightIndex;
                e.controllerSignal = (uint)rightState.ulButtonPressed;
                e.padX = rightState.rAxis0.x;
                e.padY = rightState.rAxis0.y;
                e.controller = rightController;
                e.isLeft = false;
                OnRightMenuPressed(e);
            }
            else if (menu > 0L && rightMenuPressed)
            {
                ControllerEventArgs e;
                e.controllerIndex = rightIndex;
                e.controllerSignal = (uint)rightState.ulButtonPressed;
                e.padX = rightState.rAxis0.x;
                e.padY = rightState.rAxis0.y;
                e.controller = rightController;
                e.isLeft = false;
                OnRightMenuHolding(e);
            }
            else if (menu == 0L && rightMenuPressed)
            {
                rightMenuPressed = false;
                ControllerEventArgs e;
                e.controllerIndex = rightIndex;
                e.controllerSignal = (uint)rightState.ulButtonPressed;
                e.padX = rightState.rAxis0.x;
                e.padY = rightState.rAxis0.y;
                e.controller = rightController;
                e.isLeft = false;
                OnRightMenuUnpressed(e);
            }
        }

        if (leftGripped && rightGripped && gripOption != GripReposition.NONE)
        {
            Vector3 delta = ((rightPos + leftPos) - (controllerManager.right.transform.position + controllerManager.left.transform.position)) / 2.0f;
            if (gripOption == GripReposition.HORIZONTAL_MOVE)
            {
                delta.y = 0;
            }
            //transform.position = currPos + delta * movementScale;
            transform.Translate(delta, Space.World);

            if(gripOption == GripReposition.HORIZONTAL_MOVE || gripOption == GripReposition.SIX_DOF_MOVE)
            {
                return;
            }
            Vector3 from = controllerManager.right.transform.position - controllerManager.left.transform.position;
            from.y = 0;
            Vector3 to = rightPos - leftPos;
            to.y = 0;
            float angle = Vector3.Angle(from, to);
            //Debug.Log(angle);
            if (angle < 1) angle = 0;
            Vector3 temp = Vector3.Cross(from, to);
            if (temp.y < 0) angle = -angle;
            transform.RotateAround(transform.Find("Camera (head)").position, Vector3.up, angle);
            //transform.RotateAround(Vector3.zero, Vector3.up, angle);

            float zoom = (rightPos - leftPos).magnitude - (controllerManager.right.transform.position - controllerManager.left.transform.position).magnitude;
            if (Mathf.Abs(zoom) < 0.1f) zoom = 0f;
            //transform.localScale += new Vector3(zoom, zoom, zoom);
        }

        leftPos = controllerManager.left.transform.position;
        rightPos = controllerManager.right.transform.position;
    }
}
