using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using NanoVRController;

public abstract class ControlScheme: MonoBehaviour{

    public abstract void SetController(VRController c);

    public abstract void Enable();

    public abstract void Disable();
}
