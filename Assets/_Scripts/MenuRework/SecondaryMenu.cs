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
    public static SecondaryMenu _instance;
    public Transform functionMenu;
    public Transform animationMenu;
    public Transform saveLoadMenu;
    public Transform settingsMenu;
    public Transform tournamentMenu;
    public GameObject volumeBall;

    SecondaryMenuResponder responder;

    public static SecondaryMenu GetInstance()
    {
        return _instance;
    }

    void Awake()
    {
        _instance = this;
    }

    void Start()
    {
        functionMenu.gameObject.SetActive(true);
        animationMenu.gameObject.SetActive(false);
        saveLoadMenu.GetComponentInChildren<Scroll>().setUpMenu();
        saveLoadMenu.gameObject.SetActive(false);
        settingsMenu.gameObject.SetActive(false);
        tournamentMenu.gameObject.SetActive(false);

        responder = new SecondaryMenuResponder();
        responder.initialize(this);
        GetComponent<FlexMenu>().RegisterResponder(responder);
        volumeBall.GetComponent<Collider>().enabled = false;
        volumeBall.GetComponent<MeshRenderer>().enabled = false;

        transform.parent.gameObject.SetActive(false);
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
                tournamentMenu.gameObject.SetActive(false);
                break;
            case "Animation":
                functionMenu.gameObject.SetActive(false);
                animationMenu.gameObject.SetActive(true);
                saveLoadMenu.gameObject.SetActive(false);
                settingsMenu.gameObject.SetActive(false);
                tournamentMenu.gameObject.SetActive(false);
                break;
            case "SaveLoad":
                functionMenu.gameObject.SetActive(false);
                animationMenu.gameObject.SetActive(false);
                saveLoadMenu.gameObject.SetActive(true);
                settingsMenu.gameObject.SetActive(false);
                tournamentMenu.gameObject.SetActive(false);
                break;
            case "Settings":
                functionMenu.gameObject.SetActive(false);
                animationMenu.gameObject.SetActive(false);
                saveLoadMenu.gameObject.SetActive(false);
                settingsMenu.gameObject.SetActive(true);
                tournamentMenu.gameObject.SetActive(false);
                break;
            case "Matryx":
                functionMenu.gameObject.SetActive(false);
                animationMenu.gameObject.SetActive(false);
                saveLoadMenu.gameObject.SetActive(false);
                settingsMenu.gameObject.SetActive(false);
                tournamentMenu.gameObject.SetActive(true);
                //tournamentMenu.GetComponent<TournamentMenu>().LoadTournaments();
                break;
        }
    }

    void Update() { }
}
