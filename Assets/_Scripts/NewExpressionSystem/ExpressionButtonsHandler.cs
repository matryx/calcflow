using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ExpressionButtonsHandler : MonoBehaviour {
    internal class KeyboardInputResponder : FlexMenu.FlexMenuResponder
    {
        ExpressionButtonsHandler buttonHandler;
        internal KeyboardInputResponder(ExpressionButtonsHandler del)
        {
            this.buttonHandler = del;
        }

        public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
        {
            buttonHandler.HandleInput(sender.name);
        }

        public void Flex_ActionEnd(string name, FlexActionableComponent sender, GameObject collider) { }
    }

    FlexMenu flex;
    Expressions expressions;
    private FlexMenu keyboard;
    KeyboardInputResponder responder;
    JoyStickAggregator joyStickAggregator;

    Transform popup;
    float distance = 1;

    void Start()
    {
        expressions = Expressions._instance;
        responder = new KeyboardInputResponder(this);
        flex = GetComponent<FlexMenu>();
        flex.RegisterResponder(responder);

        popup = transform.Find("DeleteConfirmation");
    }

    public void HandleInput(string buttonID)
    {
        switch (buttonID)
        {
            case "Yes":
                expressions.deleteExpression();
                popup.gameObject.SetActive(false);
                break;
            case "No":
                popup.gameObject.SetActive(false);
                break;
        }
    }

    void Update() {

    }
}
