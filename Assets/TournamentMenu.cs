using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class TournamentMenu : MonoBehaviour {

    private CalcManager calcManager;
    private MultiSelectFlexPanel tournamentsPanel;

    [SerializeField]
    private SubmissionMenu submissionMenu;

    private string[] Intro = { "A", "The"};
    private string[] Setup = { "Cure for our", "Cure for The", "Solution to The", "End of", "Quest for The", "Mission to", "Beginning of our", "Promise to Create The", "Beautiful Relationship with The" };
    private string[] adjectives = { "Untimely", "Fast-Approaching", "Promising", "Holy", "Perfect", "Incomprehensible", "Smart", "Advanced", "Crazy", "Hopeful", "Dreadful", "Rapidly Advancing", "Unstoppable", "Edible", "Uncontrollable", "Shameless", "Quantified", "Dangerous"};
    private string[] nouns = { "Zika virus", "Global Warming", "Educational Model", "Unified Quantum Theory", "Neural Implant", "Life-Harboring Planet", "Boredome", "Robot Overlords", "Star Destroyer", "Designer Baby", "Brain-Computer Interface", "Death", "Mars", "Sol", "Sexbot", "Nanobot", "Evolutionary Process", "Contact Lense", "Gene Therapy"};
    private string[] Outro = { ".", "We So Desperately Need.", "to Save Us All.", ": A Parent's Worst Nightmare.", "that Will End All Wars.", "to Revolutionize Just About Everything", ": A Journey", "that Might Kill Us All." };

    

    private Dictionary<string, Matryx_Tournament> tournaments;

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

        LoadTournaments();
        DisplayTournaments();
    }

    private void LoadTournaments()
    {
        tournaments = new Dictionary<string, Matryx_Tournament>();
        System.Random random = new System.Random();
        for (int i = 0; i < 15; i++)
        {
            int indexOne = random.Next(0, Intro.Length - 1);
            int indexTwo = random.Next(0, Setup.Length - 1);
            int indexThree = random.Next(0, adjectives.Length - 1);
            int indexFour = random.Next(0, this.nouns.Length - 1);
            int indexFive = random.Next(0, this.Outro.Length - 1);

            string intro = this.Intro[indexOne];
            string setup = this.Setup[indexTwo];
            string adjective = this.adjectives[indexThree];
            string noun = this.nouns[indexFour];
            string outro = this.Outro[indexFive];

            string description = intro + " " + setup + " " + adjective + " " + noun + " " + outro;

            Matryx_Tournament aTournament = new Matryx_Tournament(description, random.Next(10000000, 200000000).GetHashCode().ToString(), random.Next(100, 5000000));
            tournaments.Add("" + i, aTournament);
        }
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
        button.name = tournament.name;
        button.GetComponent<TournamentContainer>().SetTournament(tournament);

        string shortName = tournament.name.Length > maxTextLength ? tournament.name.Replace(tournament.name.Substring(maxTextLength), "...") : tournament.name;
        button.transform.Find("Text").GetComponent<TMPro.TextMeshPro>().text = shortName;

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
        submissionMenu.SetTournament(tournament);
    }
}
