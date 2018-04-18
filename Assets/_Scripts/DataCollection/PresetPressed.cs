using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using CalcFlowUI;
using System;
using VoxelBusters.RuntimeSerialization;

[RuntimeSerializable(typeof(MonoBehaviour), true, true)]
public class PresetPressed : QuickButton
{
    private string preset;
    private ParametricAnalyticsManager analyticsManager;

    // Use this for initialization
    protected override void Start()
    {
        base.Start();
        preset = transform.parent.gameObject.name;
        //analyticsManager = GameObject.Find("ParametricAnalytics").GetComponent<ParametricAnalyticsManager>();
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        analyticsManager.IncrementPreset(preset);
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
    }
}
