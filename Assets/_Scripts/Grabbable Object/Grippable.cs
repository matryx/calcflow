using UnityEngine;
using System.Collections;
using System.Collections.Generic;
using NanoVRController;
using TriggerForwarding;

public abstract class Grippable : MonoBehaviour, TriggerListener
{
    public enum PRIORITY
    {
        LOW,
        MEDIUM,
        HIGH
    }
    public PRIORITY priority = PRIORITY.LOW;

    protected Transform m_parent;
    protected LinkedList<Grabber> activeGrabbers = new LinkedList<Grabber>();
    protected LinkedList<Grabber> potentialGrabbers = new LinkedList<Grabber>();
    protected LinkedList<Grabber> grabbersToKeep = new LinkedList<Grabber>();

    protected bool isGrabbed = false;

    string startLayer;

    public bool IsGrabbed
    {
        get
        {
            return isGrabbed;
        }
        set
        {
            isGrabbed = value;
        }
    }

    protected virtual void Start()
    {
        startLayer = LayerMask.LayerToName(gameObject.layer);


        if (transform.parent != null)
        {
            m_parent = transform.parent;
        }
        else
        {
            m_parent = null;
        }
    }


    protected virtual void FixedUpdate()
    {
        LinkedListNode<Grabber> curr = potentialGrabbers.First;
        LinkedListNode<Grabber> temp;
        if (curr == null) return;
        while (curr != null)
        {
            temp = curr.Next;
            if (!grabbersToKeep.Contains(curr.Value))
            {
                StoppedColliding(curr.Value);
            }
            curr = temp;
        }
        grabbersToKeep.Clear();
    }

    public virtual void OnTriggerEnter(Collider c)
    {
        Grabber grabber = c.gameObject.GetComponent<Grabber>();
        if (grabber != null &&
           !potentialGrabbers.Contains(grabber))
        {
            potentialGrabbers.AddLast(grabber);
            grabber.AddGrippable(this);
        }
    }

    public virtual void OnTriggerStay(Collider c)
    {
        Grabber grabber = c.gameObject.GetComponent<Grabber>();
        if (grabber != null && !grabbersToKeep.Contains(grabber))
        {
            grabbersToKeep.AddLast(grabber);
        }
    }


    protected virtual void StoppedColliding(Grabber grabber)
    {
        potentialGrabbers.Remove(grabber);
        grabber.RemoveGrippable(this);
    }

    /// <summary>
    /// Callback function to register grabbing controller
    /// </summary>
    /// <param name="c"></param>
    /// <param name="e"></param>
    public virtual void RegisterController(Grabber g, ControllerComponentArgs e)
    {
        if (activeGrabbers.Contains(g))
        {
            Debug.Log("Error, controller grabbing twice.");
            return;
        }
        activeGrabbers.AddLast(g);
        g.controller.SendHapticEvent(0.01f, 1f, 0.03f);
        OnRegisterController(g);
        IsGrabbed = true;
    }
    /// <summary>
    /// Callback function to release a controller if already registered
    /// </summary>
    /// <param name="c"></param>
    /// <param name="e"></param>
    public virtual void ReleaseController(Grabber g, ControllerComponentArgs e)
    { 
        if (activeGrabbers.Contains(g))
        {
            activeGrabbers.Remove(g);
            //c.SendHapticEvent(1, 1, 0.5f);
            OnReleaseController(g);
        }
        IsGrabbed = false;
    }

    protected abstract void OnRegisterController(Grabber g);        //isReady = true;


    protected abstract void OnReleaseController(Grabber g);

    #region pivot
    /// <summary>
    /// Bind transform to grabbing pivot
    /// </summary>
    /// <param name="t"></param>
    /// <param name="keepParent">remember its old parent if true</param>
    protected void AttachToPivot(Transform t, bool keepParent)
    {
        if (keepParent)
        {
            if (transform.parent != null)
            {
                m_parent = transform.parent;
            }
            else
            {
                m_parent = null;
            }
        }
        transform.SetParent(t, true);
    }

    /// <summary>
    /// Unbind transform from grabbing pivot
    /// </summary>
    protected void DetachFromPivot()
    {
        transform.SetParent(m_parent, true);
    }
    #endregion

    protected virtual void OnDestroy()
    {
        if (activeGrabbers != null)
        {
            foreach (Grabber g in activeGrabbers)
            {
                g.RemoveGrippable(this);
            }
        }
    }
    #region unusedVirtual
    protected virtual void Awake() { }
    protected virtual void Update() { }
    protected virtual void LateUpdate() { }
    public virtual void OnTriggerExit(Collider c) { }


    #endregion
}
