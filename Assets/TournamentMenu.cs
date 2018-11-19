using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

using Matryx;

public class TournamentMenu : MonoBehaviour
{
    private CalcManager calcManager;
    private MultiSelectFlexPanel tournamentsPanel;

    [SerializeField]
    private SubmissionsMenu submissionsMenu;
    [SerializeField]
    private SubmitMenu submitMenu;

    private Dictionary<string, MatryxTournament> tournaments = new Dictionary<string, MatryxTournament>();

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
        MatryxExplorer.RunFetchTournaments(page, ProcessTournaments);
    }

    /// <summary>
    /// Loads the next page of tournaments.
    /// </summary>
    public void LoadMoreTournaments()
    {
        page++;
        removeLoadButton();
        MatryxExplorer.RunFetchTournaments(page, ProcessTournaments);
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
        DisplayTournaments((List<MatryxTournament>)results);
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
            submissionsMenu.SetTournament(tournament);
            submitMenu.SetTournament(tournament);
            submissionsMenu.gameObject.GetComponent<AnimationHandler>().OpenMenu();
        }
    }
}
