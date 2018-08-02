using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ToggleSecondaryMenu : QuickButton
{
    Transform secondaryMenu;
    FuseButton fuseButton;

    protected override void Start()
    {
        base.Start();
        SecondaryMenu secondary = SecondaryMenu.GetInstance();
        if (secondary != null)
        {
            secondaryMenu = secondary.transform.parent;
            fuseButton = this.transform.parent.GetComponentInChildren<FuseButton>();
        }
        else
        {
            Destroy(this.gameObject);
            return;
        }
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        if (secondaryMenu.gameObject.activeSelf)
        {
            secondaryMenu.gameObject.SetActive(false);
        }
        else
        {
            secondaryMenu.gameObject.SetActive(true);
        }
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
        //fuseButton.ForceCold();
    }
}
