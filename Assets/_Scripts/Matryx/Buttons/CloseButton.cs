using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class CloseButton : QuickButton {

    [SerializeField]
    private GameObject objectParent;

    protected override void ButtonEnterBehavior(GameObject other)
    {
        objectParent.GetComponent<AnimationHandler>().CloseMenu();
        transform.parent.GetComponent<FlexActionableComponent>().SetState(2);
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
        transform.parent.GetComponent<FlexActionableComponent>().SetState(1);
    }
}
