using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using VoxelBusters.RuntimeSerialization;

[RuntimeSerializable(typeof(MonoBehaviour), false)]
[RequireComponent(typeof(JoyStickReceiver))]

public class JoyStickForwarder : MonoBehaviour {

    private JoyStickReceiver destination;

    public JoyStickReceiver Destination
    {
        get
        {
            return destination;
        }
        set
        {
            if (destination != null)
            {
                GetComponent<JoyStickReceiver>().JoyStickTouched -= destination.ReceiverJoystickInput;
            }
            destination = value;
            GetComponent<JoyStickReceiver>().JoyStickTouched += destination.ReceiverJoystickInput;
        }
    }

    public void SetDestination(JoyStickReceiver receiver)
    {
        if (destination != null)
        {
            GetComponent<JoyStickReceiver>().JoyStickTouched -= destination.ReceiverJoystickInput;
        }
        destination = receiver;
        GetComponent<JoyStickReceiver>().JoyStickTouched += destination.ReceiverJoystickInput;
    }
}
