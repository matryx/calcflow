using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ExpressionTabHandler : MonoBehaviour {
    internal class ExpressionTabResponder : FlexMenu.FlexMenuResponder
    {
        ExpressionTabHandler menu;

        internal void Initialize(ExpressionTabHandler m)
        {
            menu = m;
        }

        public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
        {
            menu.ToggleMenu(sender.name);
        }

        public void Flex_ActionEnd(string name, FlexActionableComponent sender, GameObject collider) { }
    }

    public Transform paramPanel;
    public Transform vecFieldPanel;
    public Transform constantPanel;

    ExpressionTabResponder responder;
    OutputManager outputManager;
    Expressions expressions;

    void Start()
    {
        paramPanel.GetComponentInChildren<Scroll>().SetUpMenu();
        paramPanel.gameObject.SetActive(true);
        vecFieldPanel.GetComponentInChildren<Scroll>().SetUpMenu();
        vecFieldPanel.gameObject.SetActive(false);
        constantPanel.GetComponentInChildren<Scroll>().SetUpMenu();
        constantPanel.gameObject.SetActive(false);

        responder = new ExpressionTabResponder();
        responder.Initialize(this);
        GetComponent<FlexMenu>().RegisterResponder(responder);

        outputManager = OutputManager._instance;
        outputManager.setManager(ParametricManager._instance);

        expressions = Expressions._instance;
        expressions.SetManager(ParametricManager._instance);
    }

    void ToggleMenu(string menuName)
    {
        switch (menuName)
        {
            case "ParametrizationTab":
                paramPanel.gameObject.SetActive(true);
                vecFieldPanel.gameObject.SetActive(false);
                constantPanel.gameObject.SetActive(false);

                outputManager.setManager(ParametricManager._instance);
                expressions.SetManager(ParametricManager._instance);
                break;
            case "VectorFieldTab":
                paramPanel.gameObject.SetActive(false);
                vecFieldPanel.gameObject.SetActive(true);
                constantPanel.gameObject.SetActive(false);

                outputManager.setManager(VecFieldManager._instance);
                expressions.SetManager(VecFieldManager._instance);
                expressions.ReselectVecExpression();
                break;
            case "ConstantTab":
                paramPanel.gameObject.SetActive(false);
                vecFieldPanel.gameObject.SetActive(false);
                constantPanel.gameObject.SetActive(true);
                break;
        }
    }

    void Update() { }
}
