using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ErrorPopupHandler : MonoBehaviour {
    internal class KeyboardInputResponder : FlexMenu.FlexMenuResponder
    {
        ErrorPopupHandler buttonHandler;
        internal KeyboardInputResponder(ErrorPopupHandler del)
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
    FlexMenu keyboard;
    KeyboardInputResponder responder;

    CalcInput calcInput;

    public void HandleInput(string buttonID)
    {
        switch (buttonID)
        {
            case "Close":
                calcInput.disablePopup();
                break;
        }
    }

    void Start () {
        responder = new KeyboardInputResponder(this);
        flex = GetComponent<FlexMenu>();
        flex.RegisterResponder(responder);

        calcInput = CalcInput._instance;
    }
	
	void Update () { }
}
