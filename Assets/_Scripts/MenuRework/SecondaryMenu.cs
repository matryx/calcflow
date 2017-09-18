using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class SecondaryMenu : MonoBehaviour
{
    internal class SecondaryMenuResponder : FlexMenu.FlexMenuResponder
    {
        SecondaryMenu secMenu;

        internal void initialize(SecondaryMenu sec)
        {
            secMenu = sec;
        }

        public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
        {
            secMenu.toggleMenu(sender.name);
        }

        public void Flex_ActionEnd(string name, FlexActionableComponent sender, GameObject collider) { }
    }

    public Transform functionMenu;
    public Transform animationMenu;
    public Transform saveLoadMenu;
    Vector3 setPosition;
    Vector3 setScale;

    SecondaryMenuResponder responder;

    // Use this for initialization
    void Start()
    {
        functionMenu.gameObject.SetActive(true);
        animationMenu.gameObject.SetActive(false);
        saveLoadMenu.gameObject.SetActive(false);

        setPosition = functionMenu.localPosition;
        setScale = Vector3.one;

        responder = new SecondaryMenuResponder();
        responder.initialize(this);
        GetComponent<FlexMenu>().RegisterResponder(responder);
    }

    void toggleMenu(string menuName)
    {
        switch (menuName)
        {
            case "ExampleFunctions":
                functionMenu.gameObject.SetActive(true);
                animationMenu.gameObject.SetActive(false);
                saveLoadMenu.gameObject.SetActive(false);
                break;
            case "Animation":
                functionMenu.gameObject.SetActive(false);
                animationMenu.gameObject.SetActive(true);
                saveLoadMenu.gameObject.SetActive(false);
                break;
            case "SaveLoad":
                functionMenu.gameObject.SetActive(false);
                animationMenu.gameObject.SetActive(false);
                saveLoadMenu.gameObject.SetActive(true);
                break;
        }
    }

    // Update is called once per frame
    void Update()
    {

    }
}
