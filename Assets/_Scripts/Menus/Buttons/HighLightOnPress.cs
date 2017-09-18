using CalcFlowUI;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System;

[RequireComponent(typeof(Button))]

public class HighLightOnPress : QuickButton {
    int numPressers;

    public Color HighlightColor;
    private Color color;
    Renderer rend;

    // Use this for initialization
    protected override void Start()
    {
        base.Start();

        if (!rend)
        {
            rend = GetComponentInChildren<Renderer>();
        }
        color = rend.material.color;
    }

    // Update is called once per frame
    void LateUpdate()
    {
        if (numPressers > 0)
        {
            rend.material.color = HighlightColor;
        }
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        numPressers++;
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
        numPressers--;
    }
}
