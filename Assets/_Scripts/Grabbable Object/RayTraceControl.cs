using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using NanoVRController;
using Extensions;
using System.Linq;
using CalcFlowUI;

public class RayTraceControl : ControlScheme {
    public VRController controller;
    Grabber rayGrabber;
    GameObject selected;
    Vector3 targetPoint;
    [HideInInspector] public GameObject target;
    Collider deadCollider;
    private GameObject laser;
    private LineRenderer laserLine;
    public Material laserMat;
    public Color highlightedColor;
    private GameObject[] menuItems;
    Color initialColor;
    public LayerMask raycastLayers;

    #region callbacks
    void Grab(VRController c, ControllerComponentArgs e)
    {
        Grab();
    }

    void Grab()
    {
        if (!target) return;
        Grippable grippable = target.GetComponentInParent<Grippable>();
        if (!grippable) return;
        rayGrabber.transform.position = targetPoint;
        rayGrabber.GrabObject(grippable);
    }

    void Drop()
    {
        if (rayGrabber)
        {
            rayGrabber.DropAll();
        }
    }

    void Drop(VRController c, ControllerComponentArgs e)
    {
        Drop();
    }

    Button button;
    void ButtonPress(VRController c, ControllerComponentArgs e) {
        if (!target) return;
        button = GetComponent<RayCastButton>();
        if (button != null) button.PressButton(this.gameObject);
    }

    void ButtonUnpress(VRController c, ControllerComponentArgs e)
    {
        ButtonUnpress();
    }

    void ButtonUnpress()
    {
        if (!target) return;
        button = GetComponent<RayCastButton>();
        if (button != null) button.UnpressButton(this.gameObject);
        button = null;
    }

    public float speed = 1;
    public float minDist = .05f;

    void PushSelected(VRController c, ControllerComponentArgs e) {
        Vector3 currPos = rayGrabber.transform.position;
        Vector3 nextPos = currPos + transform.forward * e.y * speed;
        
        float angle = Vector3.Angle(transform.forward, nextPos - transform.position);


        if (Mathf.Abs(angle) < 90 )
        {
            rayGrabber.transform.position = nextPos;
        }
    }

    void RotateSelected(VRController c, ControllerComponentArgs e)
    {
        rayGrabber.transform.RotateAround(rayGrabber.transform.position, Vector3.up, e.x*4);
    }
    #endregion
    #region initializors

    private void Awake()
    {
        menuItems = GameObject.FindGameObjectsWithTag("MenuItem");
    }

    private void Start()
    {
        InitRayGrabber();
        InitLaser();
        InitButtonBall();
        Enable();
    }

    private void InitRayGrabber()
    {
        rayGrabber = new GameObject().AddComponent<Grabber>();
        rayGrabber.SetController(controller);
        rayGrabber.name = "raygrabber";
        rayGrabber.transform.parent = this.transform;
    }

    private void InitLaser()
    {
        laser = new GameObject();
        laser.transform.parent = this.transform;
        laser.name = "laser";
        laserLine = laser.AddComponent<LineRenderer>();
        laserLine.GetComponent<Renderer>().material = laserMat;
        laserLine.positionCount = 2;
        laserLine.startWidth = .01f;
        laserLine.endWidth = .01f;
        laserLine.SetPosition(0, Vector3.zero);
        laserLine.SetPosition(1, Vector3.zero);
        //laserLine.material = laserMat;
    }

    private void InitButtonBall()
    {
        GameObject buttonBall = GameObject.CreatePrimitive(PrimitiveType.Sphere);
        buttonBall.layer = 2;
        buttonBall.transform.localScale = new Vector3(.01f, .01f, .01f);
        buttonBall.GetComponent<Collider>().isTrigger = true;
        GameObject.Destroy(buttonBall.GetComponent<Renderer>());
        deadCollider = buttonBall.GetComponent<Collider>();
        buttonBall.name = "buttonBall";
        buttonBall.layer = LayerMask.NameToLayer("ButtonPresser");

        buttonBall.SetActive(false);
    }

    public override void SetController(VRController c)
    {
        controller = c;
    }

    public override void Enable()
    {
        ConnectController();
        isEnabled = true;
    }

    public override void Disable()
    {
        DisconnectController();
        isEnabled = false;
    }

    public void ConnectController()
    {
        controller.components[ButtonId.GRIP].ComponentPressed += Grab;
        controller.components[ButtonId.GRIP].ComponentUnpressed += Drop;
        controller.components[ButtonId.TRIGGER].ComponentPressed += ButtonPress;
        controller.components[ButtonId.TRIGGER].ComponentUnpressed += ButtonUnpress;
        controller.components[ButtonId.THUMBPAD].ComponentTouching += PushSelected;
        //controller.components[ButtonId.THUMBPAD].ComponentTouching += RotateSelected;
    }

    #endregion
    #region destructor
    public void DisconnectController()
    {
        Drop();
        ButtonUnpress();
        controller.components[ButtonId.GRIP].ComponentPressed -= Grab;
        controller.components[ButtonId.GRIP].ComponentUnpressed -= Drop;
        controller.components[ButtonId.TRIGGER].ComponentPressed -= ButtonPress;
        controller.components[ButtonId.TRIGGER].ComponentUnpressed -= ButtonUnpress;
        controller.components[ButtonId.THUMBPAD].ComponentTouching -= PushSelected;
        controller.components[ButtonId.THUMBPAD].ComponentTouching -= RotateSelected;
    }

    private void OnDestroy()
    {
        DisconnectController();
    }
    #endregion
    #region onUpdate
    void Update ()
    {
        updateTarget();
        updateLaser();

    }

    void updateLaser()
    {
        if (isOn())
        {
            laser.SetActive(true);
        }
        else 
        {
            laser.SetActive(false);
            return;
        }

        laserLine.SetPosition(0, transform.position);
        if (selected)
        {
            laserLine.SetPosition(1, targetPoint);
        }
        else
        {
            laserLine.SetPosition(1, transform.forward * 1000);
        }
    }

    void highlightWithRaytracer(GameObject target)
    {
        if (target.transform.parent != null)
        {
            if (target.transform.parent.GetComponent<FlexButtonComponent>() != null)
            {
                if (target.transform.parent.GetComponent<FlexButtonComponent>().State != 2)
                    target.GetComponent<Renderer>().material.color = highlightedColor;
                else
                    target.GetComponent<Renderer>().material.color = target.transform.parent.GetComponent<FlexButtonComponent>().selectedColor;
            }
        }
        else
            target.GetComponent<Renderer>().material.color = highlightedColor;
    }


    void unhighlightWithRayTracer(GameObject lastTarget, Color lastTargetColor)
    {

        if (lastTarget.transform.parent != null)
        {
            if (lastTarget.transform.parent.GetComponent<FlexButtonComponent>() != null)
            {
                if (lastTarget.transform.parent.GetComponent<FlexButtonComponent>().State != 2)
                    lastTarget.GetComponent<Renderer>().material.color = lastTargetColor;
                else
                    lastTarget.GetComponent<Renderer>().material.color = lastTarget.transform.parent.GetComponent<FlexButtonComponent>().selectedColor;
            }
        }
        else
            lastTarget.GetComponent<Renderer>().material.color = lastTargetColor;        
    }

    private void updateTarget()
    {
        GameObject lastSelected = selected;
        GameObject lastTarget = target;
        getTarget();

        if (checkTarget(lastSelected))
        {
            if (target != null && menuItems.Contains(target))
            {
                highlightWithRaytracer(target);
            }

        } 
        else
        {
            if (lastTarget != null && menuItems.Contains(lastTarget))
            {
                unhighlightWithRayTracer(lastTarget, initialColor);

            }
            if (target != null)
            {
                if(target.GetComponent<Renderer>() != null)
                    initialColor = target.GetComponent<Renderer>().material.color;
            }
        }
    }


    void getTarget()
    {
        RaycastHit hitinfo;
        if (isOn() && Physics.Raycast(transform.position, transform.forward, out hitinfo, Mathf.Infinity, raycastLayers))
        {
            selected = hitinfo.transform.gameObject;
            target = hitinfo.collider.gameObject;
            targetPoint = hitinfo.point;
        }
        else
        {
            target = null;
            selected = null;
            targetPoint = Vector3.zero;
        }
    }

    //returns true if the target hasn't changed
    bool checkTarget(GameObject lastSelected)
    {
        if (lastSelected == null || selected == null)
        {
            return false;
        }
        if (selected == lastSelected)
        {
            return true;
        }
        else
        {
            return false;
        }
    }

    bool waited = false;
    private bool WaitFrame()
    {
        if (!waited)
        {
            waited = true;
            return false;
        }
        else
        {
            waited = false;
            return true;
        }
    }
    #endregion
    public Grabber handGrabber;
    private bool isEnabled;

    public bool Enabled
    {
        get
        {
            return isEnabled;
        }
    }

    #region other
    public bool isOn()
    {
        if (handGrabber == null) return Enabled;

        return handGrabber.Enabled && !handGrabber.CanGrab();
    }
    #endregion
}
