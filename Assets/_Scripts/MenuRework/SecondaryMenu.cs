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
    public Transform settingsMenu;
    public GameObject volumeBall; 

    SecondaryMenuResponder responder;

    void Start()
    {
        functionMenu.gameObject.SetActive(true);
        animationMenu.gameObject.SetActive(false);
        saveLoadMenu.GetComponentInChildren<Scroll>().setUpMenu();
        saveLoadMenu.gameObject.SetActive(false);
        settingsMenu.gameObject.SetActive(false);

        responder = new SecondaryMenuResponder();
        responder.initialize(this);
        GetComponent<FlexMenu>().RegisterResponder(responder);
        volumeBall.GetComponent<Collider>().enabled = false;
        volumeBall.GetComponent<MeshRenderer>().enabled = false;
    }

    void toggleMenu(string menuName)
    {
        switch (menuName)
        {
            case "Presets":
                functionMenu.gameObject.SetActive(true);
                animationMenu.gameObject.SetActive(false);
                saveLoadMenu.gameObject.SetActive(false);
                settingsMenu.gameObject.SetActive(false);
                break;
            case "Animation":
                functionMenu.gameObject.SetActive(false);
                animationMenu.gameObject.SetActive(true);
                saveLoadMenu.gameObject.SetActive(false);
                settingsMenu.gameObject.SetActive(false);
                break;
            case "SaveLoad":
                functionMenu.gameObject.SetActive(false);
                animationMenu.gameObject.SetActive(false);
                saveLoadMenu.gameObject.SetActive(true);
                settingsMenu.gameObject.SetActive(false);
                break;
            case "Settings":
                functionMenu.gameObject.SetActive(false);
                animationMenu.gameObject.SetActive(false);
                saveLoadMenu.gameObject.SetActive(false);
                settingsMenu.gameObject.SetActive(true);
                break;
        }
    }

    void Update() { }
}
