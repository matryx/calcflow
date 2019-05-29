using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class CloseButton : QuickButton {

    [SerializeField]
    private GameObject objectParent;
    [SerializeField]
    private MenuStateReceiver menuStateReceiver;

    protected override void ButtonEnterBehavior(GameObject other)
    {
        objectParent.GetComponent<AnimationHandler>().CloseMenu((obj) => { menuStateReceiver?.OnMenuClosed(); });
        menuStateReceiver?.OnMenuClose();
        transform.parent.GetComponent<FlexActionableComponent>().SetState(2);
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
        transform.parent.GetComponent<FlexActionableComponent>().SetState(1);
    }
}
