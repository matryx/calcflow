using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class ReenactableAction
{
    public abstract void Reenact();
    public abstract string key { get; }

}
