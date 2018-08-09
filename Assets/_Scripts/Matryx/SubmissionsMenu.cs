using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class SubmissionsMenu : MonoBehaviour
{
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

    static Matryx_Tournament tournament;
    public static Matryx_Tournament Tournament
    {
        get
        {
            return tournament;
        }
    }

    private Dictionary<string, Matryx_Submission> submissions = new Dictionary<string, Matryx_Submission>();

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

    public void HandleInput(GameObject source)
    {
        if (source.name == "Load_Button")
        {
            LoadMoreSubmissions();
        }
        else
        {
            Matryx_Submission submission = source.GetComponent<SubmissionContainer>().GetSubmission();
            DisplaySubmissionUI(submission);
        }
    }

    int page = 0;
    public void SetTournament(Matryx_Tournament newTournament)
    {
        if (tournament == null ||
            tournament.uniqueId != newTournament.uniqueId)
        {
            tournament = newTournament;
            UpdateHeaderUI();

            ClearSubmissions();
            MatryxJsonRpc.Request.RunListSubmissions(tournament.uniqueId, page, ProcessTournament);
        }
    }

    /// <summary>
    /// Loads the next page of submissions under a tournament
    /// </summary>
    public void LoadMoreSubmissions()
    {
        page++;
        removeLoadButton();
        MatryxJsonRpc.Request.RunListSubmissions(tournament.uniqueId, page, ProcessTournament);
    }

    /// <summary>
    /// Clears the list of submissions.
    /// </summary>
    public void ClearSubmissions()
    {
        page = 0;
        submissions.Clear();
        scroll.Clear();
    }

    public void ProcessTournament(object result)
    {
        var rpcSubmissions = (List<MatryxJsonRpc.Submission>)result;
        var newSubmissions = new List<Matryx_Submission>();
        foreach (var rpcSubmission in rpcSubmissions)
        {
            var submissionAddress = rpcSubmission.address;
            var submissionTitle = rpcSubmission.title;
            var aSubmission = new Matryx_Submission(submissionTitle, submissionAddress);
            submissions.Add(submissionAddress, aSubmission);
            newSubmissions.Add(aSubmission);
        }
        DisplaySubmissions(newSubmissions);
    }

    /*
    public void ProcessTournamentOLD(string jsonString)
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
                List<Matryx_Submission> newSubmissions = new List<Matryx_Submission>();
                jsonTournament.GetField("submissions", delegate (JSONObject jsonSubmissions)
                {
                    submissionsList = jsonSubmissions.list;
                    foreach (JSONObject jsonSubmission in submissionsList)
                    {
                        string submissionAddress = jsonSubmission.GetField("address").str;
                        string submissionTitle = jsonSubmission.GetField("title").str;

                        Matryx_Submission aSubmission = new Matryx_Submission(submissionTitle, submissionAddress);
                        submissions.Add(submissionAddress, aSubmission);
                        newSubmissions.Add(aSubmission);
                    }

                    DisplaySubmissions(newSubmissions);
                });
            });
        });
    }
    */

    private void UpdateHeaderUI()
    {
        titleText.text = tournament.getTitle();
        bountyText.text = "Reward: " + tournament.getBounty() + " MTX";
        descriptionText.text = tournament.getDescription();
    }

    public void DisplaySubmissionUI(Matryx_Submission submission)
    {
        submissionMenu.SetSubmission(submission);
        submissionMenu.gameObject.GetComponent<AnimationHandler>().OpenMenu();
    }

    GameObject loadButton;
    public void DisplaySubmissions(List<Matryx_Submission> _submissions)
    {
        foreach (Matryx_Submission submission in _submissions)
        {
            GameObject button = createButton(submission);
            button.SetActive(false);
            submissionsPanel.AddAction(button.GetComponent<FlexButtonComponent>());
        }

        loadButton = createLoadButton();
        submissionsPanel.AddAction(loadButton.GetComponent<FlexButtonComponent>());
    }

    private GameObject createButton(Matryx_Submission submission)
    {
        GameObject button = Instantiate(Resources.Load("Submission_Cell", typeof(GameObject))) as GameObject;
        button.transform.SetParent(submissionsPanel.transform);
        button.transform.localScale = Vector3.one;

        button.name = submission.getTitle();
        button.GetComponent<SubmissionContainer>().SetSubmission(submission);

        button.transform.Find("Text").GetComponent<TMPro.TextMeshPro>().text = submission.getTitle();

        scroll.AddObject(button.transform);
        joyStickAggregator.AddForwarder(button.GetComponentInChildren<JoyStickForwarder>());

        return button;
    }

    private GameObject createLoadButton()
    {
        GameObject button = Instantiate(Resources.Load("Submission_Cell", typeof(GameObject))) as GameObject;
        button.transform.SetParent(submissionsPanel.transform);
        button.transform.localScale = Vector3.one;

        button.name = "Load_Button";

        button.transform.Find("Text").GetComponent<TMPro.TextMeshPro>().text = "Load More...";

        scroll.AddObject(button.transform);
        joyStickAggregator.AddForwarder(button.GetComponentInChildren<JoyStickForwarder>());

        return button;
    }

    private void removeLoadButton()
    {
        if (loadButton != null)
        {
            List<Transform> loadButtonTransform = new List<Transform>();
            loadButtonTransform.Add(loadButton.transform);
            scroll.DeleteObjects(new List<Transform>(loadButtonTransform));

            Destroy(loadButton);
        }
    }
}
