using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using VoxelBusters.RuntimeSerialization;

[RuntimeSerializable(typeof(MonoBehaviour), false)]
public class RayCastReceiver : MonoBehaviour{

    public delegate void RayCastCallback(RayCastSender sender);
    public event RayCastCallback OnRayCastStart;
    public event RayCastCallback OnRayCastStay;
    public event RayCastCallback OnRayCastEnd;

    public void RayCastStart(RayCastSender sender)
    {
        if(OnRayCastStart != null)
            OnRayCastStart.Invoke(sender);
    }

    public void RayCastStay(RayCastSender sender)
    {
        if (OnRayCastStay != null)
            OnRayCastStay.Invoke(sender);
    }

    public void RayCastEnd(RayCastSender sender)
    {
        if (OnRayCastEnd != null)
            OnRayCastEnd.Invoke(sender);
    }

}
