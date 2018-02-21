using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class DeleteExpression : QuickButton {


    //TODO:
    //need to put this into new script and attach it to button x input

    internal class KeyboardInputResponder : FlexMenu.FlexMenuResponder
    {
        DeleteExpression delExpr;
        internal KeyboardInputResponder(DeleteExpression del)
        {
            this.delExpr = del;
        }

        public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
        {
            delExpr.HandleInput(sender.name);
        }

        public void Flex_ActionEnd(string name, FlexActionableComponent sender, GameObject collider) { }
    }

    Expressions expressions;
    private FlexMenu keyboard;
    KeyboardInputResponder responder;
    JoyStickAggregator joyStickAggregator;

    Transform popup;
    float distance = 1;
    public GameObject scene_camera;

    protected override void Start()
    {
        base.Start();
        expressions = Expressions._instance;
        responder = new KeyboardInputResponder(this);
    }

    public void HandleInput(string buttonID)
    {
        switch(buttonID)
        {
            case "Yes":
                expressions.deleteExpression();
                break;
            case "No":
                Destroy(popup);
                break;
        }
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        //if (popup == null)
        //{
        //    popup = Resources.Load("DeleteConfirmation") as Transform;
        //    popup.position = scene_camera.transform.position + scene_camera.transform.forward * distance;
        //}

        expressions.deleteExpression();
    }

    protected override void ButtonExitBehavior(GameObject other) { }

    void Update() { }
}
