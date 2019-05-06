using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

using Matryx;
using UnityEngine.UI;

public class TournamentMenu : MonoBehaviour
{
    public static TournamentMenu Instance { get; private set; }

    private CalcManager calcManager;
    public MultiSelectFlexPanel submissionsPanel;

    [SerializeField]
    private SubmissionMenu submissionMenu;
    [SerializeField]
    private FlexButtonComponent continueButton;

    [SerializeField]
    private FlexPanelComponent mainPanel;

    [SerializeField]
    private TMPro.TextMeshPro timeRemainingText;
    [SerializeField]
    private TMPro.TextMeshPro bountyText;
    [SerializeField]
    private TMPro.TextMeshPro titleText;
    int titlePage = 0;
    [SerializeField]
    private TMPro.TextMeshPro creatorText;
    
    [SerializeField]
    private TMPro.TextMeshPro descriptionText;

    [SerializeField]
    private TournamentMenuCenterButton centerButton;
    [SerializeField]
    private TMPro.TextMeshPro roundText;

    [SerializeField]
    private FlexButtonComponent previousRoundButton;
    [SerializeField]
    private FlexButtonComponent nextRoundButton;

    [SerializeField]
    private GameObject winningAndMySubmissions;
    [SerializeField]
    private TMPro.TextMeshPro loadingSubmissions;

    public static MatryxTournament Tournament { get; private set; }
    public static MatryxRound Round { get; private set; }
    public static int roundIndex;
    public static int RoundIndex
    {
        get { return roundIndex; }
        set
        {
            if (value == 0) { Instance.previousRoundButton.SetState(-1); }
            else { Instance.previousRoundButton.SetState(0); }
            if (value == Tournament.currentRound.index) { Instance.previousRoundButton.SetState(-1); }
            else { Instance.previousRoundButton.SetState(0); }

            roundIndex = value;
        }
    }

    private Dictionary<string, MatryxSubmission> submissions = new Dictionary<string, MatryxSubmission>();

    public ActionState actionState;

    public enum ActionState
    {
        NoAction,
        Contribute,
        SelectWinners,
        ManageTournament
    }

    internal class TournamentMenuResponder : FlexMenu.FlexMenuResponder
    {
        TournamentMenu submissionMenu;
        internal TournamentMenuResponder(TournamentMenu submissionMenu)
        {
            this.submissionMenu = submissionMenu;
        }

        public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
        {
            submissionMenu.HandleInput(sender.gameObject);
        }

        public void Flex_ActionEnd(string name, FlexActionableComponent sender, GameObject collider)
        {
            sender.SetState(0);
        }
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
        TournamentMenuResponder responder = new TournamentMenuResponder(this);
        submissionsPanel = GetComponentInChildren<MultiSelectFlexPanel>().Initialize();
        joyStickAggregator = scroll.GetComponent<JoyStickAggregator>();

        mainPanel.AddAction(previousRoundButton);
        mainPanel.AddAction(nextRoundButton);
        flexMenu.AddPanel(mainPanel);
        flexMenu.RegisterResponder(responder);
    }

    public void HandleInput(GameObject source)
    {
        if (source.name == "Load_Button")
        {
            //LoadMoreSubmissions();
        }
        else if (source.name.Contains("Previous Round"))
        {
            SetRound(--RoundIndex);
        }
        else if (source.name.Contains("Next Round"))
        {

            SetRound(++RoundIndex);
        }
        else if (source.name.Contains("Continue"))
        {
            // Open up the Close Tournament --or-- Start New Round panel
        }
        else
        {
            SubmissionContainer submissionContainer = source.GetComponent<SubmissionContainer>();

            if (actionState == ActionState.SelectWinners && TournamentMenuCenterButton.Instance.Toggled)
            {
                // TODO: Change scroll action to grip action
                continueButton.gameObject.SetActive(submissionsPanel.selected.Count > 0);
                submissionContainer.distributionPicker.Toggle(submissionsPanel.selected.Count > 0);
            }
            else
            {
                DisplaySubmissionUI(submissionContainer.submission);
            }
        }
    }

    public void SetTournament(MatryxTournament newTournament)
    {
        if (Tournament == null ||
            Tournament.address != newTournament.address)
        {
            ClearSubmissions();
            Tournament = newTournament;

            loadingSubmissions.gameObject.SetActive(true);
            MatryxCortex.RunGetMySubmissions(Tournament, 0, ProcessMySubmissions);
            MatryxCortex.RunGetTournament(Tournament.address, true, ProcessTournament, ErrorLoadingTournament);
        }
    }

    public void ReloadSubmissions(float waitTime = 0)
    {
        ClearSubmissions();
        MatryxCortex.RunGetMySubmissions(Tournament, waitTime, ProcessMySubmissions);
    }

    public void SetRound(int roundIndex)
    {
        if (Tournament != null)
        {
            ClearSubmissions();

            loadingSubmissions.gameObject.SetActive(true);
            MatryxCortex.RunGetRound(Tournament.address, roundIndex, ProcessRound);
        }
    }

    /// <summary>
    /// Clears the list of submissions.
    /// </summary>
    public void ClearSubmissions()
    {
        submissions.Clear();
        scroll.clear();
        winningAndMySubmissions.SetActive(false);
    }

    public static int lastSubmissionKindProcessed = 0;
    public static void setSubmissionKindProcessed(int myType)
    {
        if (lastSubmissionKindProcessed == 0)
        {
            lastSubmissionKindProcessed = myType;
        }
        else if (lastSubmissionKindProcessed != myType)
        {
            lastSubmissionKindProcessed = 0;
        }
    }

    public void ProcessTournament(object result)
    {
        Tournament = (MatryxTournament)result;
        Round = Tournament.currentRound;
        RoundIndex = Tournament.currentRound.index;

        if (Tournament.currentRound.winningSubmissions.Count == 0)
        {
            if (lastSubmissionKindProcessed == 0)
            {
                loadingSubmissions.text = "No submissions to display";
            }
        }
        else
        {
            loadingSubmissions.gameObject.SetActive(false);
            DisplaySubmissions(Tournament.currentRound.winningSubmissions, "Winning Submissions");
        }

        UpdateActionState();
        DisplayTournamentInfo();
        setSubmissionKindProcessed(1);
    }

    public void UpdateActionState()
    {
        if (Round != Tournament.currentRound)
        {
            actionState = ActionState.NoAction;
        }
        else if (Tournament.currentRound.status.Equals(MatryxRound.STATE_CLOSED))
        {
            actionState = ActionState.NoAction;
        }
        else if (Tournament.owner.Equals(NetworkSettings.activeAccount, StringComparison.CurrentCultureIgnoreCase))
        {
            if (Tournament.currentRound.status.Equals(MatryxRound.STATE_OPEN))
            {
                actionState = ActionState.NoAction;
            }
            else if(Tournament.currentRound.status.Equals(MatryxRound.STATE_INREVIEW))
            {
                if(Tournament.currentRound.winningSubmissions == null)
                {
                    actionState = ActionState.SelectWinners;
                    TournamentMenuCenterButton.Instance.updateState();
                }
                else
                {
                    actionState = ActionState.ManageTournament;
                    TournamentMenuCenterButton.Instance.updateState();
                }
            }
        }
        else if(Tournament.currentRound.status.Equals(MatryxRound.STATE_OPEN))
        {
            actionState = ActionState.Contribute;
        }

        centerButton.updateState();
    }

    public void ProcessRound(object result)
    {
        Round = (MatryxRound)result;
        RoundIndex = Round.index;
        loadingSubmissions.gameObject.SetActive(false);
        UpdateActionState();
    }

    public void ProcessMySubmissions(object result)
    {
        var mySubmissions = (List<MatryxSubmission>)result;
        

        if (mySubmissions.Count == 0)
        {
            if (lastSubmissionKindProcessed == 0)
            {
                loadingSubmissions.text = "No submissions to display";
            }
        }
        else
        {
            loadingSubmissions.gameObject.SetActive(false);
            DisplaySubmissions((List<MatryxSubmission>)result, "My Submissions");
        }

        setSubmissionKindProcessed(2);
    }

    public void ErrorLoadingTournament(object result)
    {
        loadingSubmissions.text = "Something went wrong. :(";
        descriptionText.text = "This would normally be the description of the tournament you just pulled up, but right now it's just filler because something has gone wrong. Try reloading this tournament?";
    }

    private void DisplayTournamentInfo()
    {
        bountyText.text = Tournament.Bounty + " MTX";
        timeRemainingText.text = Tournament.currentRound.TimeRemaining;
        titleText.text = Tournament.title;
        creatorText.text = "by " + Utils.ellipseAddress(Tournament.owner);
        roundText.text = "Round " + Tournament.currentRound.index + "\n" + Tournament.currentRound.Details.Bounty + " MTX";

        descriptionText.text = Tournament.getDescription();
    }

    public void DisplaySubmissionUI(MatryxSubmission submission)
    {
        submissionMenu.SetSubmission(submission);
        submissionMenu.gameObject.GetComponent<AnimationHandler>().OpenMenu();
    }

    GameObject loadButton;
    public void DisplaySubmissions(List<MatryxSubmission> submissions, string listTitle)
    {
        if(submissions.Count == 0)
        {
            winningAndMySubmissions.SetActive(false);
            return;
        }

        winningAndMySubmissions.SetActive(true);
        createHeaderButton(listTitle);
        foreach (MatryxSubmission submission in submissions)
        {
            GameObject button = createButton(submission);
            button.SetActive(false);
            submissionsPanel.AddAction(button.GetComponent<FlexButtonComponent>());
        }

        // TODO: Decide: Keep this?
        //loadButton = createLoadButton();
        //submissionsPanel.AddAction(loadButton.GetComponent<FlexButtonComponent>());
    }

    private GameObject createButton(MatryxSubmission submission)
    {
        GameObject button = Instantiate(Resources.Load("Submission_Cell", typeof(GameObject))) as GameObject;
        button.transform.SetParent(submissionsPanel.transform);
        button.transform.localScale = Vector3.one;

        button.name = submission.title;
        button.GetComponent<SubmissionContainer>().submission = submission;

        button.transform.Find("Text").GetComponent<TMPro.TextMeshPro>().text = submission.title;

        scroll.addObject(button.transform);
        joyStickAggregator.AddForwarder(button.GetComponentInChildren<JoyStickForwarder>());

        return button;
    }

    private GameObject createHeaderButton(string text)
    {
        GameObject button = Instantiate(Resources.Load("Submission_Cell", typeof(GameObject))) as GameObject;
        button.transform.SetParent(submissionsPanel.transform);
        button.transform.localScale = Vector3.one;

        
        button.name = text + "Header Button";
        var tmpro = button.transform.Find("Text").GetComponent<TMPro.TextMeshPro>();
        tmpro.text = text;
        tmpro.fontSize = tmpro.fontSize * 1.5f;
        tmpro.alignment = TMPro.TextAlignmentOptions.Center;

        // Disable the button, its just for appearance
        var flexButton = button.GetComponent<FlexButtonComponent>();
        flexButton.disabledColor = Color.white;
        flexButton.SetState(-1);

        scroll.addObject(button.transform);
        joyStickAggregator.AddForwarder(button.GetComponentInChildren<JoyStickForwarder>());

        return button;
    }

    private GameObject createReloadButton()
    {
        GameObject button = Instantiate(Resources.Load("Submission_Cell", typeof(GameObject))) as GameObject;
        button.transform.SetParent(submissionsPanel.transform);
        button.transform.localScale = Vector3.one;

        button.name = "Load_Button";

        button.transform.Find("Text").GetComponent<TMPro.TextMeshPro>().text = "(Reload Tournaments)";

        scroll.addObject(button.transform);
        joyStickAggregator.AddForwarder(button.GetComponentInChildren<JoyStickForwarder>());

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

    private void Start()
    {
        if (Instance == null)
        {
            Instance = this;
        }
    }

    int counter = 0;
    private void FixedUpdate()
    {
        if (counter == 0)
        {
            titlePage = (++titlePage) % (titleText.textInfo.pageCount + 1);
            titleText.pageToDisplay = titlePage;
        }

        counter = (counter + 1) % 150;
    }
}
