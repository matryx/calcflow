using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using NanoVRController;

public class Grabber : ControlScheme {
	public VRController controller;

	List<Grippable> gripOptionsHigh = new List<Grippable>();
	List<Grippable> gripOptionsMedium = new List<Grippable>();
	List<Grippable> gripOptionsLow = new List<Grippable>();

	List<Grippable> grabbed = new List<Grippable>();

    private ControllerComponentArgs dummyArgs = new ControllerComponentArgs();

	public void Start()
	{
        if (controller)
        {
            Enable();
        }
    }

    public bool CanGrab()
    {
        return (gripOptionsLow.Count > 0) || 
               (gripOptionsMedium.Count > 0) || 
               (gripOptionsHigh.Count > 0);
    }

    public override void SetController(VRController c)
    {
        controller = c;
    }

    private bool isEnabled = false;
    public bool Enabled
    {
        get
        {
            return isEnabled;
        }
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
        controller.components[ButtonId.GRIP].ComponentUnpressed += UnGrab;
    }

    public void DisconnectController()
    {
        UnGrab();
        controller.components[ButtonId.GRIP].ComponentPressed -= Grab;
        controller.components[ButtonId.GRIP].ComponentUnpressed -= UnGrab;
    }

    public void AddGrippable(Grippable g)
	{
		switch (g.priority)
		{
			case Grippable.PRIORITY.LOW:
				gripOptionsLow.Add(g);
				break;
			case Grippable.PRIORITY.MEDIUM:
				gripOptionsMedium.Add(g);
				break;
			case Grippable.PRIORITY.HIGH:
				gripOptionsHigh.Add(g);
				break;
		}
	}

	public void RemoveGrippable(Grippable g)
	{
		switch (g.priority)
		{
			case Grippable.PRIORITY.LOW:
				gripOptionsLow.Remove(g);
				DropObject(g);
				break;
			case Grippable.PRIORITY.MEDIUM:
				gripOptionsMedium.Remove(g);
				DropObject(g);
				break;
			case Grippable.PRIORITY.HIGH:
				gripOptionsHigh.Remove(g);
				DropObject(g);
				break;
		}
	}

	protected bool GrabAll(List<Grippable> gripOptions, VRController c, ControllerComponentArgs e)
	{
		if (gripOptions.Count == 0) return false;

		foreach (Grippable g in gripOptions)
		{
			GrabObject(g, e);
		}
		return true;
	}

	protected void GrabObject(Grippable g, ControllerComponentArgs e)
	{
		g.RegisterController(this, e);
		grabbed.Add(g);
	}

	public void GrabObject(Grippable g)
	{
		GrabObject(g, dummyArgs);
	}

	protected void Grab(VRController c, ControllerComponentArgs e)
	{
		if (GrabAll(gripOptionsHigh, c, e))
		{
			return;
		}
		else if (GrabAll(gripOptionsMedium, c, e))
		{
			return;
		}

		GrabAll(gripOptionsLow, c, e);

	}

	public void DropObject(Grippable g)
	{
		DropObject(g, dummyArgs);
	}

	protected void DropObject(Grippable g, ControllerComponentArgs e)
	{
		g.ReleaseController(this, e);
		grabbed.Remove(g);
	}

    public void DropAll()
    {
        UnGrab(controller, dummyArgs); 
    }

    private void UnGrab(VRController c, ControllerComponentArgs e)
    {
        foreach (Grippable g in grabbed)
        {
            g.ReleaseController(this, e);
        }
        grabbed = new List<Grippable>();
    }

    private void UnGrab()
    {
        UnGrab(controller, dummyArgs);
    }
}
