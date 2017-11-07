using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Web;

public class TournamentMenu : MonoBehaviour {

    private CalcManager calcManager;
    private MultiSelectFlexPanel tournamentsPanel;

    [SerializeField]
    private SubmissionsMenu submissionsMenu;
    private string tournamentsEndpoint = "http://13.57.11.64/v1/tournaments/";

    private string[] Intro = { "A", "The"};
    private string[] Setup = { "Cure for our", "Cure for The", "Solution to The", "End of", "Quest for The", "Mission to", "Beginning of our", "Promise to Create The", "Beautiful Relationship with The" };
    private string[] adjectives = { "Untimely", "Fast-Approaching", "Promising", "Holy", "Perfect", "Incomprehensible", "Smart", "Advanced", "Crazy", "Hopeful", "Dreadful", "Rapidly Advancing", "Unstoppable", "Edible", "Uncontrollable", "Shameless", "Quantified", "Dangerous"};
    private string[] nouns = { "Zika virus", "Global Warming", "Educational Model", "Unified Quantum Theory", "Neural Implant", "Life-Harboring Planet", "Boredome", "Robot Overlords", "Star Destroyer", "Designer Baby", "Brain-Computer Interface", "Death", "Mars", "Sol", "Sexbot", "Nanobot", "Evolutionary Process", "Contact Lense", "Gene Therapy"};
    private string[] Outro = { ".", "We So Desperately Need.", "to Save Us All.", ": A Parent's Worst Nightmare.", "that Will End All Wars.", "to Revolutionize Just About Everything", ": A Journey", "that Might Kill Us All." };

    

    private Dictionary<string, Matryx_Tournament> tournaments = new Dictionary<string, Matryx_Tournament>();

    internal class KeyboardInputResponder : FlexMenu.FlexMenuResponder
    {
        TournamentMenu tournamentMenu;
        internal KeyboardInputResponder(TournamentMenu tournamentMenu)
        {
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
        KeyboardInputResponder responder = new KeyboardInputResponder(this);
        flexMenu.RegisterResponder(responder);
        tournamentsPanel = GetComponentInChildren<MultiSelectFlexPanel>().Initialize();
        joyStickAggregator = scroll.GetComponent<JoyStickAggregator>();
    }

    public void LoadTournaments()
    {
        if(tournaments.Keys.Count != 0)
        {
            return;
        }

        WebLoader.Instance.Load(tournamentsEndpoint, ProcessTournaments);
    }

    private void ProcessTournaments(string jsonString)
    {
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
                }

                DisplayTournaments();
            });
        });
    }

    private void DisplayTournaments()
    {
        List<Transform> toAdd = new List<Transform>();
        foreach (Matryx_Tournament tournament in tournaments.Values)
        {
            GameObject button = createButton(tournament);
            button.SetActive(false);
            tournamentsPanel.AddAction(button.GetComponent<FlexButtonComponent>());
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
        string name = source.name;

        Matryx_Tournament tournament = source.GetComponent<TournamentContainer>().GetTournament();
        submissionsMenu.SetTournament(tournament);
        submissionsMenu.gameObject.GetComponent<AnimationHandler>().OpenMenu();
    }
}
