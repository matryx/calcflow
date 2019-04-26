using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class DeactivateMainMenu : QuickButton
{
    private FuseButton fuseButton;

    // Use this for initialization
    protected override void Start()
    {
        base.Start();
        fuseButton = this.transform.parent.GetComponentInChildren<FuseButton>();
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
        fuseButton.ForceCold();
    }
}
