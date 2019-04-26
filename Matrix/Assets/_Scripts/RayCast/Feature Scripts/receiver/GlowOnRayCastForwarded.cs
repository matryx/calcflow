using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[RequireComponent(typeof(RayCastReceiver))]

public class GlowOnRayCastForwarded : MonoBehaviour
{
    int numPressers;
    int layerIndex;
    public bool recursive;
    public GameObject target;

    void Awake()
    {
        RayCastReceiver receiver = GetComponent<RayCastReceiver>();
        receiver.OnRayCastStart += OnRayCastStart;
        receiver.OnRayCastEnd += OnRayCastEnd;
        receiver.OnRayCastStay += OnRayCastStay;
    }



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
    void Start()
    {
        layerIndex = target.layer;
    }

    // Update is called once per frame
    private void LateUpdate()
    {
        if (numPressers >= 1)
        {
            if (recursive)
            {
                RecursiveChangeLayer(transform, LayerMask.NameToLayer("Glow"));
            }
            else
            {
                target.layer = LayerMask.NameToLayer("Glow");
            }
        }
        else
        {
            if (recursive)
            {
                RecursiveChangeLayer(transform, layerIndex);
            }
            else
            {
                target.layer = layerIndex;
            }
        }
    }

    public void RecursiveChangeLayer(Transform transform, int toLayer)
    {
        target.layer = toLayer;

        int childCount = transform.childCount;
        for (int i = 0; i < childCount; i++)
        {
            RecursiveChangeLayer(transform.GetChild(i), toLayer);
        }
    }
}