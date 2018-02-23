using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class DeleteExpression : QuickButton {

    Expressions expressions;
    private FlexMenu keyboard;
    JoyStickAggregator joyStickAggregator;

    Transform popup;
    float distance = 1;
    public GameObject scene_camera;

    //TODO:
    //make popup in front of meny instead of eyes so you dont have to use camera

    protected override void Start()
    {
        base.Start();
        expressions = Expressions._instance;
        popup = transform.parent.parent.parent.Find("DeleteConfirmation");
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        if (!popup.gameObject.activeSelf)
        {
            popup.gameObject.SetActive(true);
            popup.position = scene_camera.transform.position + scene_camera.transform.forward * distance;
        }

        //expressions.deleteExpression();
    }

    protected override void ButtonExitBehavior(GameObject other) { }

    void Update() { }
}
