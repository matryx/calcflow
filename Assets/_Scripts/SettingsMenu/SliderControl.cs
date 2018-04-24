using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class SliderControl : QuickButton{

    public ConstraintGrabbable slider;
    List<GameObject> pressers = new List<GameObject>(2);

    protected override void ButtonEnterBehavior(GameObject other)
    {
        pressers.Add(other);
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
        pressers.Remove(other);
    }

    protected void ButtonStayBehavior(GameObject other)
    {
        RayCastSender sender = other.GetComponent<RayCastSender>();
        if (sender == null) return;
        Vector3 point = sender.TargetPoint;
        slider.lastLocalPos = slider.sliderOrigin.InverseTransformPoint(point);
    }

    private void Update()
    {
        foreach (GameObject g in pressers)
        {
            ButtonStayBehavior(g);
        }
    }
}
