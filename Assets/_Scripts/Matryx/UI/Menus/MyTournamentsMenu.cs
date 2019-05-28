using Matryx;
using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class MyTournamentsMenu : MonoBehaviour
{
    public static MyTournamentsMenu Instance { get; private set; }
    private MultiSelectFlexPanel myTournamentsPanel;
    [SerializeField]
    TournamentMenu tournamentMenu;
    [SerializeField]
    TournamentMenuCenterButton centerButton;
    [SerializeField]
    private TMPro.TextMeshPro myTournamentsListText;
    [SerializeField]
    FlexButtonComponent flexButtonComponent;

    private Scroll scroll;
    JoyStickAggregator joyStickAggregator;
    FlexMenu flexMenu;

    private Dictionary<string, MatryxTournament> tournaments = new Dictionary<string, MatryxTournament>();

    internal class CommitButtonResponder : FlexMenu.FlexMenuResponder
    {
        public FlexMenu menu;
        MyTournamentsMenu myTournamentsMenu;
        internal CommitButtonResponder(MyTournamentsMenu myTournaments, FlexMenu menu)
        {
            this.menu = menu;
            this.myTournamentsMenu = myTournaments;
        }

        public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
        {
            myTournamentsMenu.HandleInput(sender.gameObject);
        }

        public void Flex_ActionEnd(string name, FlexActionableComponent sender, GameObject collider) {}
    }

    public void Awake()
    {
        if (Instance == null)
        {
            Instance = this;
            Instance.Initialize();
        }
    }

    public void Initialize()
    {
        scroll = GetComponentInChildren<Scroll>(true);
        flexMenu = GetComponent<FlexMenu>();
        CommitButtonResponder responder = new CommitButtonResponder(this, flexMenu);
        flexMenu.RegisterResponder(responder);
        myTournamentsPanel = GetComponentInChildren<MultiSelectFlexPanel>().Initialize();
        joyStickAggregator = scroll.GetComponent<JoyStickAggregator>();
    }

    public void ClearCommits()
    {
        page = 0;
        tournaments.Clear();
        scroll.clear();
    }

    int page = 0;
    bool loaded
    {
        get { return tournaments.Count != 0; }
    }
    bool loading = false;
    public bool LoadMyTournaments(int thePage)
    {
        if ((loaded | loading) == true) return false;
        loading = true;
        myTournamentsListText.text = "Loading Tournaments...";
        ClearCommits();
        MatryxCortex.RunGetMyTournaments(0, 0, ProcessTournaments, ShowError);
        return true;
    }

    /// <summary>
    /// Loads the next page of tournaments.
    /// </summary>
    public void LoadMoreMyTournaments()
    {
        loading = false;
        if (LoadMyTournaments(page + 1))
        {
            page++;
        }
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
        var resultingTournaments = (List<MatryxTournament>)results;
        myTournamentsListText.text = "My Tournaments";
        DisplayTournaments(resultingTournaments);
    }

    private void ShowError(object results)
    {
        myTournamentsListText.text = "Unable to Load Tournaments";
    }

    GameObject loadButton;
    private void DisplayTournaments(List<MatryxTournament> _tournaments)
    {
        foreach (MatryxTournament tournament in _tournaments)
        {
            if (tournaments.ContainsKey(tournament.address)) continue;
            tournaments.Add(tournament.address, tournament);
            GameObject button = createButton(tournament);
            button.SetActive(false);
            myTournamentsPanel.AddAction(button.GetComponent<FlexButtonComponent>());
        }

        loadButton = createLoadButton();
    }

    private GameObject createLoadButton()
    {
        GameObject button = Instantiate(Resources.Load("Tournament_Cell", typeof(GameObject))) as GameObject;
        button.transform.SetParent(myTournamentsPanel.transform);
        button.transform.localScale = Vector3.one;
        button.transform.position = new Vector3(-500f, -500f, -500f);

        button.name = "Load_Button";
        var text = button.transform.Find("Text").GetComponent<TMPro.TextMeshPro>();
        text.text = "Reload Tournaments";
        text.fontStyle = TMPro.FontStyles.Bold;
        text.alignment = TMPro.TextAlignmentOptions.Center;

        TMPro.TextMeshPro matryxBountyTMP = button.transform.Find("MTX_Amount").GetComponent<TMPro.TextMeshPro>();
        matryxBountyTMP.text = "";

        scroll.addObject(button.transform);
        joyStickAggregator.AddForwarder(button.GetComponentInChildren<JoyStickForwarder>());

        // Add this action to the panel
        myTournamentsPanel.AddAction(button.GetComponent<FlexButtonComponent>());

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
        button.transform.SetParent(myTournamentsPanel.transform);
        button.transform.localScale = Vector3.one;

        button.name = tournament.title;
        var tournamentContainer = button.GetComponent<TournamentContainer>();
        tournamentContainer.tournament = tournament;
        tournamentContainer.SetStatus(tournament.currentRound.status);

        button.transform.Find("Text").GetComponent<TMPro.TextMeshPro>().text = tournament.title;

        TMPro.TextMeshPro matryxBountyTMP = button.transform.Find("MTX_Amount").GetComponent<TMPro.TextMeshPro>();
        matryxBountyTMP.text = tournament.Bounty + " " + matryxBountyTMP.text;

        scroll.addObject(button.transform);
        joyStickAggregator.AddForwarder(button.GetComponentInChildren<JoyStickForwarder>());

        return button;
    }


    private void HandleInput(GameObject source)
    {
        if (source.name == "Load_Button")
        {
            tournaments.Clear();
            LoadMoreMyTournaments();
        }
        else if (source.GetComponent<TournamentContainer>())
        {
            string name = source.name;

            MatryxTournament tournament = source.GetComponent<TournamentContainer>().tournament;
            // TODO: Navigate the user to the corresponding tournament through the menus
            tournamentMenu.gameObject.GetComponent<AnimationHandler>().OpenMenu();
            tournamentMenu.SetTournament(tournament);
            centerButton.transform.parent.gameObject.SetActive(false);
        }
    }
}