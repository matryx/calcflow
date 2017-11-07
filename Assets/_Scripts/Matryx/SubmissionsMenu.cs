using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Web;

public class SubmissionsMenu : MonoBehaviour {

    private CalcManager calcManager;
    private MultiSelectFlexPanel submissionsPanel;
    private string tournamentEndpoint = "http://13.57.11.64/v1/tournament/";

    [SerializeField]
    private SubmissionMenu submissionMenu;

    [SerializeField]
    private TMPro.TextMeshPro titleText;
    [SerializeField]
    private TMPro.TextMeshPro bountyText;
    [SerializeField]
    private TMPro.TextMeshPro descriptionText;

    Matryx_Tournament tournament;
    private Dictionary<string, Matryx_Submission> submissions = new Dictionary<string, Matryx_Submission>();


    private string[] Intro = { "A", "The" };
    private string[] Setup = { "Cure for our", "Cure for The", "Solution to The", "End of", "Quest for The", "Mission to", "Beginning of our", "Promise to Create The", "Beautiful Relationship with The" };
    private string[] adjectives = { "Untimely", "Fast-Approaching", "Promising", "Holy", "Perfect", "Incomprehensible", "Smart", "Advanced", "Crazy", "Hopeful", "Dreadful", "Rapidly Advancing", "Unstoppable", "Edible", "Uncontrollable", "Shameless", "Quantified", "Dangerous" };
    private string[] nouns = { "Zika virus", "Global Warming", "Educational Model", "Unified Quantum Theory", "Neural Implant", "Life-Harboring Planet", "Boredome", "Robot Overlords", "Star Destroyer", "Designer Baby", "Brain-Computer Interface", "Death", "Mars", "Sol", "Sexbot", "Nanobot", "Evolutionary Process", "Contact Lense", "Gene Therapy" };
    private string[] Outro = { ".", "We So Desperately Need.", "to Save Us All.", ": A Parent's Worst Nightmare.", "that Will End All Wars.", "to Revolutionize Just About Everything", ": A Journey", "that Might Kill Us All." };

    internal class KeyboardInputResponder : FlexMenu.FlexMenuResponder
    {
        SubmissionsMenu submissionMenu;
        internal KeyboardInputResponder(SubmissionsMenu submissionMenu)
        {
            this.submissionMenu = submissionMenu;
        }

        public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
        {
            submissionMenu.HandleInput(sender.gameObject);
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
        submissionsPanel = GetComponentInChildren<MultiSelectFlexPanel>().Initialize();
        joyStickAggregator = scroll.GetComponent<JoyStickAggregator>();
    }

    public void HandleInput(GameObject sender)
    {
        Matryx_Submission submission = sender.GetComponent<SubmissionContainer>().GetSubmission();
        DisplaySubmissionUI(submission);
    }

    public void SetTournament(Matryx_Tournament tournament)
    {
        
        if(this.tournament == null || 
            this.tournament.address != tournament.address)
        {
            this.tournament = tournament;

            scroll.clear();
            WebLoader.Instance.Load(tournamentEndpoint + "?id=" + tournament.address, ProcessTournament);
        }
    }

    public void ProcessTournament(string jsonString)
    {
        JSONObject jsonObject = new JSONObject(jsonString);
        jsonObject.GetField("results", delegate (JSONObject results)
        {
            results.GetField(tournament.address, delegate (JSONObject jsonTournament)
            {
                var title = jsonTournament.GetField("title").str;
                var bounty = jsonTournament.GetField("bounty").f;
                var description = jsonTournament.GetField("description").str;

                tournament.title = title;
                tournament.bounty = new long?((long)bounty);
                tournament.description = description;
                UpdateHeaderUI();

                List<JSONObject> submissionsList = null;
                jsonTournament.GetField("submissions", delegate (JSONObject jsonSubmissions)
                {
                    submissionsList = jsonSubmissions.list;
                    foreach (JSONObject jsonSubmission in submissionsList)
                    {
                        string submissionAddress = jsonSubmission.GetField("address").str;
                        string submissionTitle = jsonSubmission.GetField("title").str;

                        Matryx_Submission aSubmission = new Matryx_Submission(submissionTitle, submissionAddress);
                        submissions.Add(submissionAddress, aSubmission);
                    }

                    DisplaySubmissions();
                });
            });
        });
    }

    private void UpdateHeaderUI()
    {
        titleText.text = tournament.getTitle();
        bountyText.text = "" + tournament.getBounty() + " MTX";
        descriptionText.text = "\t" + tournament.getDescription();
    }

    public void DisplaySubmissionUI(Matryx_Submission submission)
    {
        //TODO: Write
        submissionMenu.SetSubmission(submission);
        submissionMenu.gameObject.GetComponent<AnimationHandler>().OpenMenu();
    }

    public void DisplaySubmissions()
    {
        List<Transform> toAdd = new List<Transform>();
        foreach (Matryx_Submission submission in submissions.Values)
        {
            GameObject button = createButton(submission);
            button.SetActive(false);
            submissionsPanel.AddAction(button.GetComponent<FlexButtonComponent>());
        }
    }

    private GameObject createButton(Matryx_Submission submission)
    {
        GameObject button = Instantiate(Resources.Load("Submission_Cell", typeof(GameObject))) as GameObject;
        button.transform.SetParent(submissionsPanel.transform);
        button.transform.localScale = Vector3.one;

        button.name = submission.getTitle();
        button.GetComponent<SubmissionContainer>().SetSubmission(submission);

        string shortName = submission.getTitle().Length > maxTextLength ? submission.getTitle().Replace(submission.getTitle().Substring(maxTextLength), "...") : submission.getTitle();
        button.transform.Find("Text").GetComponent<TMPro.TextMeshPro>().text = shortName;

        scroll.addObject(button.transform);
        joyStickAggregator.AddForwarder(button.GetComponentInChildren<JoyStickForwarder>());

        return button;
    }
}
