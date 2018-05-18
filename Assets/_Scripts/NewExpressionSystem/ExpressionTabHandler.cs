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
    public Transform linTransPanel;
    public Transform longCube;

    ExpressionTabResponder responder;

    void Start()
    {
        paramPanel.GetComponentInChildren<Scroll>().setUpMenu();
        paramPanel.gameObject.SetActive(true);
        vecFieldPanel.GetComponentInChildren<Scroll>().setUpMenu();
        vecFieldPanel.gameObject.SetActive(false);
        constantPanel.GetComponentInChildren<Scroll>().setUpMenu();
        constantPanel.gameObject.SetActive(false);
        linTransPanel.GetComponentInChildren<Scroll>().setUpMenu();
        linTransPanel.gameObject.SetActive(false);

        responder = new ExpressionTabResponder();
        responder.initialize(this);
        GetComponent<FlexMenu>().RegisterResponder(responder);
    }

    void toggleMenu(string menuName)
    {
        switch (menuName)
        {
            case "ParametrizationTab":
                paramPanel.gameObject.SetActive(true);
                vecFieldPanel.gameObject.SetActive(false);
                constantPanel.gameObject.SetActive(false);
                linTransPanel.gameObject.SetActive(false);
                longCube.gameObject.SetActive(false);
                break;
            case "VectorFieldTab":
                paramPanel.gameObject.SetActive(false);
                vecFieldPanel.gameObject.SetActive(true);
                constantPanel.gameObject.SetActive(false);
                linTransPanel.gameObject.SetActive(false);
                longCube.gameObject.SetActive(false);
                break;
            case "ConstantTab":
                paramPanel.gameObject.SetActive(false);
                vecFieldPanel.gameObject.SetActive(false);
                constantPanel.gameObject.SetActive(true);
                linTransPanel.gameObject.SetActive(false);
                longCube.gameObject.SetActive(false);
                break;
            case "LinearTransTab":
                paramPanel.gameObject.SetActive(false);
                vecFieldPanel.gameObject.SetActive(false);
                constantPanel.gameObject.SetActive(false);
                linTransPanel.gameObject.SetActive(true);
                longCube.gameObject.SetActive(true);
                break;
        }
    }

    void Update() { }
}
