using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ExpressionActions : QuickButton
{
    Transform delete;
    Transform toggleHide;

    private void Initialize()
    {
        delete = transform.parent.Find("Delete");
        toggleHide = transform.parent.Find("ToggleHide");
    }

    protected override void Start()
    {
        base.Start();
        Initialize();
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {

    }

    protected override void ButtonExitBehavior(GameObject other)
    {

    }

    void Update() { }
}
