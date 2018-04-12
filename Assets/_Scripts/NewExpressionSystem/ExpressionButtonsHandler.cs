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

    IEnumerator ScaleTo(Transform obj, Vector3 start, Vector3 end, float overTime)
    {
        float startTime = Time.time;

        while (Time.time < startTime + overTime)
        {
            obj.localScale = Vector3.Lerp(start, end, (Time.time - startTime) / overTime);
            yield return null;
        }

        obj.localScale = end;
        if (end == Vector3.zero) obj.gameObject.SetActive(false);
    }

    public void HandleInput(string buttonID)
    {
        switch (buttonID)
        {
            case "Yes":
                expressions.deleteExpression(transform.GetComponent<ExpressionComponent>().getExpressionParent());
                break;
            case "No":
                break;
        }

        StartCoroutine(ScaleTo(popup, Vector3.one, Vector3.zero, 0.1f));
    }

    void Update() {

    }
}
