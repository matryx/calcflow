using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class DeleteExpression : QuickButton {

    Expressions expressions;
    private FlexMenu keyboard;
    JoyStickAggregator joyStickAggregator;

    Transform popup;
    float distance = -4;
    Transform deleteButton;

    protected override void Start()
    {
        base.Start();
        expressions = Expressions._instance;
        popup = transform.parent.parent.parent.Find("DeleteConfirmation");
        deleteButton = transform.parent;
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        if (!popup.gameObject.activeSelf)
        {
            popup.gameObject.SetActive(true);
            popup.position = deleteButton.position + deleteButton.forward * distance;
        }
    }

    protected override void ButtonExitBehavior(GameObject other) { }

    void Update() { }
}
