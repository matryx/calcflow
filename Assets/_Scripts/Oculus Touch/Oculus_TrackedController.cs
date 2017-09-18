using UnityEngine;
using System.Collections;
//using OvrTouch.Controllers;

public struct Oculus_ControllerArgs
{
    public float axis_1d;
    public float axis_x;
    public float axis_y;
}

public delegate void Oculus_ControllerAction(object sender, Oculus_ControllerArgs e);

public class Oculus_TrackedController : MonoBehaviour
{
    //public HandednessId handedness;

    // button
    bool button1Press = false; // X/A
    bool button2Press = false; // B/Y
    bool thumbPress = false;
    bool startPress = false;

    // axis
    float handTrigger = 0f;
    float indexTrigger = 0f;
    float joystickX = 0f, joystickY = 0f;

    // touch
    bool indexTouch = false;
    bool thumbTouch = false;

    OVRInput.Controller controllerType;
    Transform trackedTransform;

    public event Oculus_ControllerAction button1Pressed;
    public event Oculus_ControllerAction button2Pressed;
    public event Oculus_ControllerAction indexTriggerTouched;
    public event Oculus_ControllerAction indexTriggerTouching;
    public event Oculus_ControllerAction indexTriggerUntouched;
    public event Oculus_ControllerAction handTriggerPressed;
    public event Oculus_ControllerAction handTriggerPressing;
    public event Oculus_ControllerAction handTriggerUnpressed;
    public event Oculus_ControllerAction handTriggerUnpressing;
    public event Oculus_ControllerAction joystickPush;
    public event Oculus_ControllerAction thumbPressed;
    public event Oculus_ControllerAction thumbUnpressed;
    public event Oculus_ControllerAction thumbTouched;
    public event Oculus_ControllerAction thumbUntouched;
    public event Oculus_ControllerAction indexTriggerPressed;
    public event Oculus_ControllerAction indexTriggerPress;
    public event Oculus_ControllerAction indexTriggerHolding;
    public event Oculus_ControllerAction indexTriggerUnpress;
    public event Oculus_ControllerAction indexTriggerUnpressed;
    public event Oculus_ControllerAction menubuttonPressed;

    void Start()
    {
        OVRCameraRig cameraRig = FindObjectOfType<OVRCameraRig>();
        //controllerType = (handedness == HandednessId.Left) ? OVRInput.Controller.LTouch : OVRInput.Controller.RTouch;
        //trackedTransform = (handedness == HandednessId.Left) ? cameraRig.leftHandAnchor : cameraRig.rightHandAnchor;
        // reset righthandanchor local position which is somehow non-zero after teleporting back to intro scene
        trackedTransform.localPosition = Vector3.zero;
        trackedTransform.rotation = Quaternion.identity;
        transform.SetParent(trackedTransform);

        GameOptions.initializedControllerCount++;
        if (GameOptions.initializedControllerCount >= 2)
        {
            if (GameOptions.initialCameraPos != Vector3.zero)
            //if (GameOptions.initialCameraPos != Vector3.zero && handedness == HandednessId.Right)
            {
                Transform cameraTransform = transform.parent.parent.parent.parent;
                if (cameraTransform != null)
                {
                    cameraTransform.position = GameOptions.initialCameraPos;
                    cameraTransform.rotation = GameOptions.initialCameraRot;
                    //GameOptions.initialCameraPos = Vector3.zero;
                }
            }
            GameOptions.initializedControllerCount = 0;
        }
    }

    #region Callbacks
    void OnButton1Pressed(Oculus_ControllerArgs e)
    {
        if (button1Pressed != null)
        {
            button1Pressed(this, e);
        }
    }
    void OnButton2Pressed(Oculus_ControllerArgs e)
    {
        if (button2Pressed != null)
        {
            button2Pressed(this, e);
        }
    }
    void OnThumbPressed(Oculus_ControllerArgs e)
    {
        if(thumbPressed != null)
        {
            thumbPressed(this, e);
        }
    }
    void OnThumbUnpressed(Oculus_ControllerArgs e)
    {
        if(thumbUnpressed != null)
        {
            thumbUnpressed(this, e);
        }
    }
    void OnThumbTouched(Oculus_ControllerArgs e)
    {
        if (thumbTouched != null)
        {
            thumbTouched(this, e);
        }
    }
    void OnThumbUntouched(Oculus_ControllerArgs e)
    {
        if(thumbUntouched != null)
        {
            thumbUntouched(this, e);
        }
    }
    void OnIndexTriggerTouched(Oculus_ControllerArgs e)
    {
        if (indexTriggerTouched != null)
        {
            indexTriggerTouched(this, e);
        }
    }
    void OnIndexTriggerUntouched(Oculus_ControllerArgs e)
    {
        if(indexTriggerUntouched != null)
        {
            indexTriggerUntouched(this, e);
        }
    }
    void OnIndexTriggerPressed(Oculus_ControllerArgs e)
    {
        if(indexTriggerPressed != null)
        {
            indexTriggerPressed(this, e);
        }
    }
    void OnIndexTriggerPress(Oculus_ControllerArgs e)
    {
        if(indexTriggerPress != null)
        {
            indexTriggerPress(this, e);
        }
    }
    void OnIndexTriggerHolding(Oculus_ControllerArgs e)
    {
        if (indexTriggerHolding != null)
        {
            indexTriggerHolding(this, e);
        }
    }
    void OnIndexTriggerUnpress(Oculus_ControllerArgs e)
    {
        if(indexTriggerUnpress != null)
        {
            indexTriggerUnpress(this, e);
        }
    }
    void OnIndexTriggerUnpressed(Oculus_ControllerArgs e)
    {
        if(indexTriggerUnpressed != null)
        {
            indexTriggerUnpressed(this, e);
        }
    }
    void OnHandTriggerPressed(Oculus_ControllerArgs e)
    {
        if(handTriggerPressed != null)
        {
            handTriggerPressed(this, e);
        }
    }
    void OnHandTriggerPressing(Oculus_ControllerArgs e)
    {
        if(handTriggerPressing != null)
        {
            handTriggerPressing(this, e);
        }
    }
    void OnHandTriggerUnpressing(Oculus_ControllerArgs e)
    {
        if(handTriggerUnpressing != null)
        {
            handTriggerUnpressing(this, e);
        }
    }
    void OnJoystickPush(Oculus_ControllerArgs e)
    {
        if(joystickPush != null)
        {
            joystickPush(this, e);
        }
    }
    void OnMenubuttonPressed(Oculus_ControllerArgs e)
    {
        if(menubuttonPressed != null)
        {
            menubuttonPressed(this, e);
        }
    }
    #endregion

    void Update()
    {
        Oculus_ControllerArgs e;
        //transform.position = trackedTransform.position;
        //////Vector3 p = trackedTransform.position;
        //Vector3 p = transform.localPosition;
        //Quaternion q = new Quaternion();
        //if (handedness == HandednessId.Left)
        //{
        //    q.eulerAngles = new Vector3(trackedTransform.rotation.eulerAngles.x, trackedTransform.rotation.eulerAngles.y, trackedTransform.rotation.eulerAngles.z + 90f);
        //    p = new Vector3(p.x - 0.02f, p.y - 0.03f, p.z - 0.03f);
        //}
        //else
        //{
        //    q.eulerAngles = new Vector3(trackedTransform.rotation.eulerAngles.x, trackedTransform.rotation.eulerAngles.y, trackedTransform.rotation.eulerAngles.z - 90f);
        //    p = new Vector3(p.x + 0.02f, p.y - 0.03f, p.z - 0.03f);
        //}
        //transform.rotation = q;
        //transform.localPosition = p;

        bool button1_press = OVRInput.Get(OVRInput.Button.One, controllerType);
        if(!button1Press && button1_press)
        {
            e.axis_1d = e.axis_x = e.axis_y = 0f;
            OnButton1Pressed(e);    
        }
        button1Press = button1_press;

        bool button2_press = OVRInput.Get(OVRInput.Button.Two, controllerType);
        if(!button2Press && button2_press)
        {
            e.axis_1d = e.axis_x = e.axis_y = 0f;
            OnButton2Pressed(e);
        }
        button2Press = button2_press;

        bool thumb_press = OVRInput.Get(OVRInput.Button.PrimaryThumbstick, controllerType);
        if(!thumbPress && thumb_press)
        {
            e.axis_1d = e.axis_x = e.axis_y = 0f;
            OnThumbPressed(e);
        }
        else if(thumbPress && !thumb_press)
        {
            e.axis_1d = e.axis_x = e.axis_y = 0f;
            OnThumbUnpressed(e);
        }
        thumbPress = thumb_press;

        bool thumb_touch = OVRInput.Get(OVRInput.Touch.PrimaryThumbRest, controllerType) || OVRInput.Get(OVRInput.Touch.PrimaryThumbstick, controllerType) ||
                           OVRInput.Get(OVRInput.Touch.One, controllerType) || OVRInput.Get(OVRInput.Touch.Two, controllerType);
        if(!thumbTouch && thumb_touch)
        {
            e.axis_1d = e.axis_x = e.axis_y = 0f;
            OnThumbTouched(e);
        }
        else if(thumbTouch && !thumb_touch)
        {
            e.axis_1d = e.axis_x = e.axis_y = 0f;
            OnThumbUntouched(e);
        }
        thumbTouch = thumb_touch;

        bool index_touch = OVRInput.Get(OVRInput.Touch.PrimaryIndexTrigger, controllerType);
        if(!indexTouch && index_touch)
        {
            e.axis_1d = e.axis_x = e.axis_y = 0f;
            OnIndexTriggerTouched(e);
        }
        else if(indexTouch && !index_touch)
        {
            e.axis_1d = e.axis_x = e.axis_y = 0f;
            OnIndexTriggerUntouched(e);
        }
        indexTouch = index_touch;

        float index_press = OVRInput.Get(OVRInput.Axis1D.PrimaryIndexTrigger, controllerType);
        if(indexTrigger == 0f && index_press > 0f)
        {
            e.axis_1d = index_press;
            e.axis_x = e.axis_y = 0f;
            OnIndexTriggerPressed(e);
        }
        else if(indexTrigger > 0f && index_press > indexTrigger)
        {
            e.axis_1d = index_press;
            e.axis_x = e.axis_y = 0f;
            OnIndexTriggerPress(e);
        }
        else if(indexTrigger > 0f && index_press == indexTrigger)
        {
            e.axis_1d = index_press;
            e.axis_x = e.axis_y = 0f;
            OnIndexTriggerHolding(e);
        }
        else if(indexTrigger > 0f && index_press < indexTrigger)
        {
            e.axis_1d = index_press;
            e.axis_x = e.axis_y = 0f;
            OnIndexTriggerUnpress(e);
        }
        else if( indexTrigger > 0f && index_press == 0f)
        {
            e.axis_1d = index_press;
            e.axis_x = e.axis_y = 0f;
            OnIndexTriggerUnpressed(e);
        }
        indexTrigger = index_press;

        float hand_trigger = OVRInput.Get(OVRInput.Axis1D.PrimaryHandTrigger, controllerType);
        if(hand_trigger > 0 && handTrigger == 0f)
        {
            e.axis_1d = hand_trigger;
            e.axis_x = e.axis_y = 0f;
            OnHandTriggerPressed(e);
        }
        else if(hand_trigger > 0 && hand_trigger > handTrigger)
        {
            e.axis_1d = hand_trigger;
            e.axis_x = e.axis_y = 0f;
            OnHandTriggerPressing(e);
        }
        else if(hand_trigger > 0 && hand_trigger < handTrigger)
        {
            e.axis_1d = hand_trigger;
            e.axis_x = e.axis_y = 0f;
            OnHandTriggerUnpressing(e);
        }
        handTrigger = hand_trigger;

        Vector2 v = OVRInput.Get(OVRInput.Axis2D.PrimaryThumbstick, controllerType);
        if(v.magnitude >= 0.1f)
        {
            //float angle = Mathf.Atan2(v.x, v.y);
            e.axis_1d = 0f;
            e.axis_x = v.x;
            e.axis_y = v.y;
            OnJoystickPush(e);
        }
        joystickX = v.x;
        joystickY = v.y;

        bool start_press = OVRInput.Get(OVRInput.Button.Start, controllerType);
        if(!startPress && start_press)
        {
            e.axis_1d = e.axis_x = e.axis_y = 0f;
            OnMenubuttonPressed(e);
        }
        //else if(thumbPress && !thumb_press)
        //{
        //    e.axis_1d = e.axis_x = e.axis_y = 0f;
        //    OnThumbUnpressed(e);
        //}
        startPress = start_press;
    }
}
