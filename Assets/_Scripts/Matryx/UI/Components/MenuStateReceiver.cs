using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class MenuStateReceiver : MonoBehaviour {

    public virtual void OnMenuOpen() { }
    public virtual void OnMenuClose() { }
    public virtual void OnMenuClosed() { }
    public virtual void OnMenuLoad() { }
    public virtual void OnMenuUnload() { }
}
