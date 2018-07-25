using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ExpressionTabHandler : MonoBehaviour {
    internal class ExpressionTabResponder : FlexMenu.FlexMenuResponder
    {
        ExpressionTabHandler menu;

        internal void initialize(ExpressionTabHandler m)
        {
            menu = m;
        }

        public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
        {
            menu.toggleMenu(sender.name);
        }

        public void Flex_ActionEnd(string name, FlexActionableComponent sender, GameObject collider) { }
    }

    public Transform paramPanel;
    public Transform vecFieldPanel;
    public Transform constantPanel;

    ExpressionTabResponder responder;
    OutputManager outputManager;

    void Start()
    {
        paramPanel.GetComponentInChildren<Scroll>().setUpMenu();
        paramPanel.gameObject.SetActive(true);
        vecFieldPanel.GetComponentInChildren<Scroll>().setUpMenu();
        vecFieldPanel.gameObject.SetActive(false);
        constantPanel.GetComponentInChildren<Scroll>().setUpMenu();
        constantPanel.gameObject.SetActive(false);

        responder = new ExpressionTabResponder();
        responder.initialize(this);
        GetComponent<FlexMenu>().RegisterResponder(responder);

        outputManager = OutputManager._instance;
    }

    void toggleMenu(string menuName)
    {
        switch (menuName)
        {
            case "ParametrizationTab":
                paramPanel.gameObject.SetActive(true);
                vecFieldPanel.gameObject.SetActive(false);
                constantPanel.gameObject.SetActive(false);

                outputManager.setManager(ParametricManager._instance);
                break;
            case "VectorFieldTab":
                paramPanel.gameObject.SetActive(false);
                vecFieldPanel.gameObject.SetActive(true);
                constantPanel.gameObject.SetActive(false);

                outputManager.setManager(VecFieldManager._instance);
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
