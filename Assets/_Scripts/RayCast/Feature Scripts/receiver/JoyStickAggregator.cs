using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using VoxelBusters.RuntimeSerialization;

[RuntimeSerializable(typeof(MonoBehaviour), false)]
[RequireComponent(typeof(JoyStickReceiver))]

public class JoyStickAggregator : MonoBehaviour {

    JoyStickReceiver myReceiver;

    private void Initialize()
    {
        myReceiver = GetComponent<JoyStickReceiver>();
        foreach (JoyStickForwarder jsf in GetComponentsInChildren<JoyStickForwarder>())
        {
            jsf.Destination = myReceiver;
        }
    }

    private void Start()
    {
        if (myReceiver == null)
        {
            Initialize();
        }
    }

    public void AddForwarder(JoyStickForwarder forwarder)
    {
        if (myReceiver == null)
        {
            Initialize();
        }
        if (forwarder != null)
            forwarder.SetDestination(myReceiver);
    }

}
