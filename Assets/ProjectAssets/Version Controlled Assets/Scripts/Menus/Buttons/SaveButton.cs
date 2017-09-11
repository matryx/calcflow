using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class SaveButton : QuickButton {

    public ExpressionSaveLoad saver;

    protected override void Start()
    {
        base.Start();
        saver = GetComponent<ExpressionSaveLoad>();
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        saver.SaveDefault();
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
    }
}
