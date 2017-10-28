using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class CloseButton : QuickButton {

    [SerializeField]
    private GameObject objectParent;

    protected override void ButtonEnterBehavior(GameObject other) {}

    protected override void ButtonExitBehavior(GameObject other)
    {
        objectParent.GetComponent<AnimationHandler>().CloseMenu();
    }
}
