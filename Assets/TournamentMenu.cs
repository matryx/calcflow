using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class TournamentMenu : MonoBehaviour
{
    private CalcManager calcManager;
    private MultiSelectFlexPanel tournamentsPanel;

    [SerializeField]
    private SubmissionsMenu submissionsMenu;
    [SerializeField]
    private SubmitMenu submitMenu;
    private string tournamentsEndpoint = "http://13.57.11.64/v1/tournaments/?page=";

    private Dictionary<string, Matryx_Tournament> tournaments = new Dictionary<string, Matryx_Tournament>();

    internal class KeyboardInputResponder : FlexMenu.FlexMenuResponder
    {
        public FlexMenu menu;
        TournamentMenu tournamentMenu;
        internal KeyboardInputResponder(TournamentMenu tournamentMenu, FlexMenu menu)
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

    int page = 0;
    public void LoadTournaments()
    {
        ClearTournaments();
        MatryxJsonRpc.Request.RunListTournaments(page, ProcessTournaments);
    }

    /// <summary>
    /// Loads the next page of tournaments.
    /// </summary>
    public void LoadMoreTournaments()
    {
        page++;
        removeLoadButton();
        MatryxJsonRpc.Request.RunListTournaments(page, ProcessTournaments);
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
        var rpcTournaments = (List<MatryxJsonRpc.Tournament>)results;
        var newTournaments = new List<Matryx_Tournament>();
        foreach (var rpcTournament in rpcTournaments)
        {
            var unique = rpcTournament.UniqueId;
            var title = rpcTournament.title;
            var bounty = rpcTournament.bounty;
            Matryx_Tournament aTournament = new Matryx_Tournament(unique, title, bounty);
            aTournament.description = rpcTournament.description;
            tournaments.Add(unique, aTournament);
            newTournaments.Add(aTournament);
        }
        DisplayTournaments(newTournaments);
    }

    /*
    private void ProcessTournamentsOLD(string jsonString)
    {
        List<Matryx_Tournament> newTournaments = new List<Matryx_Tournament>();
        JSONObject jsonObject = new JSONObject(jsonString);
        jsonObject.GetField("results", delegate (JSONObject results)
        {
            List<JSONObject> jsonTournaments = null;
            results.GetField("tournaments", delegate (JSONObject tournamentList)
            {
                jsonTournaments = tournamentList.list;
                foreach(JSONObject jsonTournament in jsonTournaments)
                {
                    string address = jsonTournament.GetField("address").str;
                    string title = jsonTournament.GetField("title").str;
                    long bounty = jsonTournament.GetField("bounty").i;

                    Matryx_Tournament aTournament = new Matryx_Tournament(address, title, bounty);
                    tournaments.Add(address, aTournament);
                    newTournaments.Add(aTournament);
                }

                DisplayTournaments(newTournaments);
            });
        });
    }
    */

    GameObject loadButton;
    private void DisplayTournaments(List<Matryx_Tournament> _tournaments)
    {
        List<Transform> toAdd = new List<Transform>();
        foreach (Matryx_Tournament tournament in _tournaments)
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
        button.transform.Find("Text").GetComponent<TMPro.TextMeshPro>().text = "Load More...";

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

    private GameObject createButton(Matryx_Tournament tournament)
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

            Matryx_Tournament tournament = source.GetComponent<TournamentContainer>().GetTournament();
            submissionsMenu.SetTournament(tournament);
            submitMenu.SetTournament(tournament);
            submissionsMenu.gameObject.GetComponent<AnimationHandler>().OpenMenu();
        }

        // @stats
        // matryx tournament menu
        Calcflow.UserStatistics.StatisticsTracking.InstantEvent("Button Click", "Tournament Menu",
        new Dictionary<string, object>()
        {
            {"Button Name", source.name}
        });
    }
}
