using UnityEngine;
using System.Collections;
using NanoVRController;


public abstract class Tool : Grippable {
    public Transform holster;
    private bool active = false;
    [SerializeField] protected Transform tip;
    [SerializeField] protected int numModes = 1;
    [SerializeField] string[] modeNames;
    protected int currMode = 0;

    // override these functions to customize your tool.
    #region Virtual Functions

    // called when the tool is grabbed.
    protected virtual void OnSelect()
    {

    }

    // called when the tool is released.
    protected virtual void OnDeselect()
    {

    }

    public virtual void triggerPressOperation(VRController c, ControllerComponentArgs e)
    { 
        active = true;
    }

    public virtual void triggerHoldOperation(VRController c, ControllerComponentArgs e)
    {
        active = true;
    }

    public virtual void triggerReleaseOperation(VRController c, ControllerComponentArgs e)
    {
        active = false;
    }

    protected virtual void triggerNotHeld()
    {
        
    }

    public virtual void stickLeftOperation(VRController c, ControllerComponentArgs e)
    {
        switchModeDown();
        triggerReleaseOperation(c,e);
    }

    public virtual void stickRightOperation(VRController c, ControllerComponentArgs e)
    {
        switchModeUp();
        triggerReleaseOperation(c, e);
    }

    public virtual void stickUpOperation(VRController c, ControllerComponentArgs e)
    {

    }

    public virtual void stickDownOperation(VRController c, ControllerComponentArgs e)
    {

    }

    #endregion

    //returns the tool to the holster. Called on update.
    private void moveToHolster()
    {
        if (holster == null) return;
        transform.SetParent(holster.transform, true);
        transform.position = Vector3.MoveTowards(transform.position, holster.position, .01f);
        transform.rotation = Quaternion.RotateTowards(transform.rotation, holster.rotation, 1);

    }

    //drives the joystick callbacks.
    Vector2 lastStick = Vector2.zero;

    // <summary>
    // calls appropriate joy stick call backs when joystick input is given.
    // </summary>
    public virtual void joyStickOperation(VRController c, ControllerComponentArgs e)
    {

        if (lastStick.x < .5 && e.x > .5f)
        {
            stickLeftOperation(c,e);
        }
        if (lastStick.x > -.5 && e.x < -.5f)
        {
            stickRightOperation(c, e);
        }

        if (lastStick.y < .5 && e.y > .5f)
        {
            stickUpOperation(c, e);
        }
        if (lastStick.y > -.5 && e.y < -.5f)
        {
            stickDownOperation(c, e);
        }
        lastStick = new Vector2(e.x, e.y);
    }

    //This section manages how modes are changed.
    #region Modes
    // <summary>
    // Cyles up through modes rolls over after last mode.
    // </summary>
    protected virtual void switchModeUp()
    {
        currMode = (++currMode) % numModes;
    }

    // <summary>
    // Cyles down through modes rolls over after first mode.
    // </summary>
    protected virtual void switchModeDown()
    {
        // C# handles mod function weird so addition is required.
        currMode = (currMode + numModes - 1) % numModes;
    }
    #endregion

    // This section is not needed to make a new tool.
    #region driving mechanics
    protected override void OnReleaseController(Grabber g)
    {
        VRController c = g.controller;
        // removes the tools callbacks from the controllers event system.
        c.components[ButtonId.TRIGGER].ComponentPressed -= triggerPressOperation;
        c.components[ButtonId.TRIGGER].ComponentPressing -= triggerHoldOperation;
        c.components[ButtonId.TRIGGER].ComponentUnpressed -= triggerReleaseOperation;

        // if grabbed by 2 hands at once, attaches controller to hand that is still grabbing.
        if (activeGrabbers.Count == 1)
        {
            AttachToPivot(activeGrabbers.First.Value.transform, false);
        } // if the releasing hand was the only one grabbing, detaches the tool and transfers momentum.
        else if (activeGrabbers.Count == 0)
        {
            DetachFromPivot();
            if (GetComponent<Rigidbody>() != null)
            {
                Rigidbody r = GetComponent<Rigidbody>();
                r.velocity = c.Velocity;
                r.angularVelocity = c.AngularVelocity;
            }
            isGrabbed = false;
            triggerReleaseOperation(c, new ControllerComponentArgs());
            DetachFromPivot();
            OnDeselect();
        }
    }


    protected override void OnRegisterController(Grabber g)
    {
        VRController c = g.controller;
        isGrabbed = true;
        AttachToPivot(c.transform, true);
        // rotates the tool so it is being held properly
        transform.position = transform.parent.position;
        transform.rotation = transform.parent.rotation;
        // register the tools callback functions to the hands event system.
        c.components[ButtonId.TRIGGER].ComponentPressed += triggerPressOperation;
        c.components[ButtonId.TRIGGER].ComponentPressing += triggerHoldOperation;
        c.components[ButtonId.TRIGGER].ComponentUnpressed += triggerReleaseOperation;
        c.components[ButtonId.THUMBPAD].ComponentTouching += joyStickOperation;
        // call callback for tools on grab.
        OnSelect();
    }

    protected override void Update()
    {
        base.Update();
        //causes the tool to drift to its holster when released.
        if (!isGrabbed)
        {
            active = false;
            moveToHolster();
        }
        else
        {
            // callback for when tool is held but not triggered.
            if(!active)
               triggerNotHeld(); 
        }
    }


    protected override void Start()
    {
        base.Start();
        modeNames = new string[numModes];
    }
    #endregion

}
