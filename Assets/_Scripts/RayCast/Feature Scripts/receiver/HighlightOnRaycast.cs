using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using VoxelBusters.RuntimeSerialization;

[RuntimeSerializable(typeof(MonoBehaviour), false)]
[RequireComponent(typeof(RayCastReceiver))]

public class HighlightOnRaycast : MonoBehaviour {
    int numPressers;

    void Awake()
    {
        RayCastReceiver receiver = GetComponent<RayCastReceiver>();
        receiver.OnRayCastStart += OnRayCastStart;
        receiver.OnRayCastEnd += OnRayCastEnd;
        receiver.OnRayCastStay += OnRayCastStay;
    }

    public Color HighlightColor;
    private Color color; 
    Renderer rend;

    void OnRayCastEnd(RayCastSender sender)
    {
        numPressers--;
    }

    void OnRayCastStart(RayCastSender sender)
    {
        numPressers++;
    }

    void OnRayCastStay(RayCastSender sender)
    {
    }

    // Use this for initialization
    void Start () {
        if (!rend)
        {
            rend = GetComponentInChildren<Renderer>();
        }
        color = rend.material.color;
    }

    // Update is called once per frame
    void Update () {
        if (numPressers < 1)
        {
            rend.material.color = color;
        }
        if (numPressers > 0)
        {
            rend.material.color = HighlightColor;
        }
    }
}
