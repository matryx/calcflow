using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

using Matryx;
using System.Numerics;

public class TournamentsMenu : MonoBehaviour
{
    public static TournamentsMenu Instance { get; private set; }
    public TournamentMenuState state;

    private CalcManager calcManager;
    private MultiSelectFlexPanel tournamentsPanel;
    [SerializeField]
    TMPro.TextMeshPro selectTournamentText;
    [SerializeField]
    private UnlockAccountButton accountButton;
    [SerializeField]
    private TournamentMenu tournamentMenu;
    [SerializeField]
    private SubmitMenu submitMenu;
    [SerializeField]
    private TMPro.TextMeshPro loadingText;

    private Dictionary<string, MatryxTournament> tournaments = new Dictionary<string, MatryxTournament>();

    public enum TournamentMenuState
    {
        AccountUnlockRequired,
        WaitingForUnlock,
        DisplayingTournaments
    }

    internal class KeyboardInputResponder : FlexMenu.FlexMenuResponder
    {
        public FlexMenu menu;
        TournamentsMenu tournamentMenu;
        internal KeyboardInputResponder(TournamentsMenu tournamentMenu, FlexMenu menu)
        {
            this.menu = menu;
            this.tournamentMenu = tournamentMenu;
        }

        public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
        {
            tournamentMenu.HandleInput(sender.gameObject);
        }

        public void Flex_ActionEnd(string name, FlexActionableComponent sender, GameObject collider) { }
    }

    private Scroll scroll;
    const int maxTextLength = 400;

    JoyStickAggregator joyStickAggregator;
    FlexMenu flexMenu;

    public void Awake()
    {
        if (Instance == null)
        {
            Instance = this;
        }
    }

    public void Initialize(CalcManager calcManager)
    {
        this.calcManager = calcManager;

        scroll = GetComponentInChildren<Scroll>(true);
        flexMenu = GetComponent<FlexMenu>();
        KeyboardInputResponder responder = new KeyboardInputResponder(this, flexMenu);
        flexMenu.RegisterResponder(responder);
        tournamentsPanel = GetComponentInChildren<MultiSelectFlexPanel>().Initialize();
        joyStickAggregator = scroll.GetComponent<JoyStickAggregator>();
    }

    public void Prepare()
    {

        if(NetworkSettings.declinedAccountUnlock == null )
        {
            SetState(TournamentMenuState.AccountUnlockRequired);
        }
        else if(NetworkSettings.declinedAccountUnlock.Value)
        {
            SetState(TournamentMenuState.WaitingForUnlock);
        }
        else
        {
            SetState(TournamentMenuState.DisplayingTournaments);
        }
    }

    public static void SetState(TournamentMenuState state)
    {
        Instance.state = state;

        switch (Instance.state)
        {
            case TournamentMenuState.AccountUnlockRequired:
                Instance.ClearTournaments();
                Instance.selectTournamentText.text = "Account Unlock Required";
                Instance.accountButton.transform.parent.gameObject.SetActive(true);
                Instance.accountButton.SetButtonToUnlock();
                break;
            case TournamentMenuState.WaitingForUnlock:
                Instance.ClearTournaments();
                Instance.selectTournamentText.text = "Lift Headset to Unlock Account";
                Instance.accountButton.transform.parent.gameObject.SetActive(true);
                Instance.accountButton.SetButtonToCancel();
                break;
            case TournamentMenuState.DisplayingTournaments:
                Instance.selectTournamentText.text = "Select a Tournament";
                Instance.accountButton.transform.parent.gameObject.SetActive(false);
                Instance.LoadTournaments();
                break;
            default:
                break;
        }
    }

    int page = 0;
    public void LoadTournaments()
    {
       loadingText.gameObject.SetActive(true);
       ClearTournaments();
       MatryxExplorer.RunFetchTournaments(page, ProcessTournaments);
    }

    /// <summary>
    /// Loads the next page of tournaments.
    /// </summary>
    public void LoadMoreTournaments()
    {
        page++;
        removeLoadButton();
        LoadTournaments();
    }

    /// <summary>
    /// Clears the list of tournaments.
    /// </summary>
    public void ClearTournaments()
    {
        page = 0;
        tournaments.Clear();
        scroll.clear();
    }

    private void ProcessTournaments(object results)
    {
        loadingText.gameObject.SetActive(false);
        DisplayTournaments((List<MatryxTournament>)results);
    }

    GameObject loadButton;
    private void DisplayTournaments(List<MatryxTournament> _tournaments)
    {
        List<Transform> toAdd = new List<Transform>();
        foreach (MatryxTournament tournament in _tournaments)
        {
            GameObject button = createButton(tournament);
            button.SetActive(false);
            tournamentsPanel.AddAction(button.GetComponent<FlexButtonComponent>());
        }

        loadButton = createLoadButton();
    }

    private GameObject createLoadButton()
    {
        GameObject button = Instantiate(Resources.Load("Tournament_Cell", typeof(GameObject))) as GameObject;
        button.transform.SetParent(tournamentsPanel.transform);
        button.transform.localScale = Vector3.one;

        button.name = "Load_Button";
        button.transform.Find("Text").GetComponent<TMPro.TextMeshPro>().text = "(Reload Tournaments)";

        TMPro.TextMeshPro matryxBountyTMP = button.transform.Find("MTX_Amount").GetComponent<TMPro.TextMeshPro>();
        matryxBountyTMP.text = "";

        scroll.addObject(button.transform);
        joyStickAggregator.AddForwarder(button.GetComponentInChildren<JoyStickForwarder>());

        // Add this action to the panel
        tournamentsPanel.AddAction(button.GetComponent<FlexButtonComponent>());

        return button;
    }

    private void removeLoadButton()
    {
        if (loadButton != null)
        {
            List<Transform> loadButtonTransform = new List<Transform>();
            loadButtonTransform.Add(loadButton.transform);
            scroll.deleteObjects(new List<Transform>(loadButtonTransform));

            Destroy(loadButton);
        }
    }

    private GameObject createButton(MatryxTournament tournament)
    {
        GameObject button = Instantiate(Resources.Load("Tournament_Cell", typeof(GameObject))) as GameObject;
        button.transform.SetParent(tournamentsPanel.transform);
        button.transform.localScale = Vector3.one;

        button.name = tournament.title;
        button.GetComponent<TournamentContainer>().SetTournament(tournament);

        button.transform.Find("Text").GetComponent<TMPro.TextMeshPro>().text = tournament.title;

        TMPro.TextMeshPro matryxBountyTMP = button.transform.Find("MTX_Amount").GetComponent<TMPro.TextMeshPro>();
        matryxBountyTMP.text = tournament.bounty + " " + matryxBountyTMP.text;

        scroll.addObject(button.transform);
        joyStickAggregator.AddForwarder(button.GetComponentInChildren<JoyStickForwarder>());

        return button;
    }

    private void HandleInput(GameObject source)
    {
        if (source.name == "Load_Button")
        {
            LoadMoreTournaments();
        }
        else if (source.GetComponent<TournamentContainer>() != null)
        {
            string name = source.name;

            MatryxTournament tournament = source.GetComponent<TournamentContainer>().GetTournament();
            tournamentMenu.SetTournament(tournament);
            submitMenu.SetTournament(tournament);
            tournamentMenu.gameObject.GetComponent<AnimationHandler>().OpenMenu();
        }
    }
}
