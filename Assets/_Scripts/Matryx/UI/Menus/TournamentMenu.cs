using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

using Matryx;
using UnityEngine.UI;
using System.Numerics;
using Vector3 = UnityEngine.Vector3;

public class TournamentMenu : MonoBehaviour
{
    public static TournamentMenu Instance { get; private set; }

    private CalcManager calcManager;
    public MultiSelectFlexPanel submissionsPanel;

    [SerializeField]
    private SubmissionMenu submissionMenu;
    [SerializeField]
    public FlexButtonComponent continueButton;
    [SerializeField]
    public ManageTournamentMenu manageTournamentMenu;

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

    string userAddress = "";

    private Dictionary<string, MatryxSubmission> submissions = new Dictionary<string, MatryxSubmission>();
    private Dictionary<string, bool> processing = new Dictionary<string, bool>();

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
        TournamentMenu tournamentMenu;
        internal TournamentMenuResponder(TournamentMenu tournamentMenu)
        {
            this.tournamentMenu = tournamentMenu;
        }

        public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
        {
            tournamentMenu.HandleInput(sender.gameObject);
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
        submissionsPanel.selectedUnselectedColor = QuickButton.TOGGLE_OFF;
        submissionsPanel.selectedSelectedColor = QuickButton.TOGGLE_ON;
        submissionsPanel.hoverUnselectedColor = QuickButton.LIGHT_HOVERING;
        submissionsPanel.hoverSelectedColor = QuickButton.DARK_HOVERING;
        submissionsPanel.passiveUnselectedColor = Color.white;
        submissionsPanel.passiveSelectedColor = QuickButton.DARK_PASSIVE;

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

            if (actionState == ActionState.SelectWinners && centerButton.Toggled)
            {
                continueButton.gameObject.SetActive(submissionsPanel.selected.Count > 0);
                submissionContainer.distributionPicker.Toggle(submissionsPanel.selected.ContainsKey(source.name));
            }
            else
            {
                DisplaySubmissionUI(submissionContainer.submission);
            }
        }
    }

    public void SetTournament(MatryxTournament theTournament)
    {
        if (Tournament == null || Tournament.address != theTournament.address || userAddress != NetworkSettings.currentAddress)
        {
            ClearSubmissions();
            // Show us something at least
            PreprocessTournament(theTournament);
            userAddress = NetworkSettings.currentAddress;

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

    IEnumerator closeResultsMenuCoroutine(float waitTime = 0)
    {
        yield return new WaitForSeconds(waitTime);
        ResultsMenu.Instance.gameObject.SetActive(false);
    }

    public void SetRound(int roundIndex)
    {
        if (Tournament != null)
        {
            ClearSubmissions();

            loadingSubmissions.gameObject.SetActive(true);
            MatryxCortex.RunGetRound(Tournament, roundIndex, ProcessRound);
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

    public static Dictionary<string, int> lastSubmissionKindProcessed = new Dictionary<string, int>();
    public static void setSubmissionKindProcessed(string tournamentAddress, int submissionType)
    {
        if (!lastSubmissionKindProcessed.ContainsKey(tournamentAddress))
        {
            lastSubmissionKindProcessed[tournamentAddress] = submissionType;
            return;
        }

        if (lastSubmissionKindProcessed[tournamentAddress] == 0)
        {
            lastSubmissionKindProcessed[tournamentAddress] = submissionType;
        }
        else if (lastSubmissionKindProcessed[tournamentAddress] != submissionType)
        {
            lastSubmissionKindProcessed[tournamentAddress] = 0;
        }
    }

    public static int getKindOfSubmissionLastProcessed(string tournamentAddress)
    {
        if (!lastSubmissionKindProcessed.ContainsKey(tournamentAddress))
        {
            lastSubmissionKindProcessed[tournamentAddress] = 0;
        }

        return lastSubmissionKindProcessed[tournamentAddress];
    }

    public void PreprocessTournament(MatryxTournament tournament)
    {
        Tournament = tournament;
        Round = Tournament.currentRound;
        loadingSubmissions.text = "Loading Submissions...";
        DisplayTournamentInfo();
    }

    public void ProcessTournament(object result)
    {
        processing[((MatryxTournament)result).address] = false;

        // Check preprocess tournament against resulting tournament
        if (Tournament.address != ((MatryxTournament)result).address)
        {
            return;
        }

        Tournament = (MatryxTournament)result;
        Round = Tournament.currentRound;
        RoundIndex = Tournament.currentRound.index;

        if (Tournament.currentRound.winningSubmissions.Count == 0 &&
            Tournament.currentRound.allSubmissions.Count == 0)
        {
            if (lastSubmissionKindProcessed[Tournament.address] == 0)
            {
                loadingSubmissions.text = "No submissions to display";
            }
        }
        else
        {
            loadingSubmissions.gameObject.SetActive(false);

            if (Tournament.currentRound.winningSubmissions.Count > 0)
            {
                DisplaySubmissions(Tournament.currentRound.winningSubmissions, "Winning Submissions");
            }

            if (Tournament.currentRound.allSubmissions.Count > 0)
            {
                DisplaySubmissions(Tournament.currentRound.allSubmissions, "All Submissions");
            }
        }

        UpdateActionState();
        DisplayTournamentInfo();
        setSubmissionKindProcessed(Tournament.address, 1);
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
        else if (Tournament.owner.Equals(NetworkSettings.currentAddress, StringComparison.CurrentCultureIgnoreCase))
        {
            if (Tournament.currentRound.status.Equals(MatryxRound.STATE_OPEN))
            {
                actionState = ActionState.NoAction;
            }
            else if (Tournament.currentRound.status.Equals(MatryxRound.STATE_INREVIEW))
            {
                if(Tournament.currentRound.winningSubmissions.Count == 0)
                {
                    actionState = ActionState.SelectWinners;
                }
                else
                {
                    actionState = ActionState.ManageTournament;
                }
            }
            else if (Tournament.currentRound.status.Equals(MatryxRound.STATE_HASWINNERS))
            {
                actionState = ActionState.ManageTournament;
            }
        }
        else if(Tournament.currentRound.status.Equals(MatryxRound.STATE_OPEN))
        {
            actionState = ActionState.Contribute;
        }

        centerButton.updateState();
    }

    public void ProcessRound(object result=null)
    {
        if (result != null)
        {
            Round = (MatryxRound)result;
        }
        else
        {
            Round = Tournament.currentRound;
        }
        
        RoundIndex = Round.index;
        loadingSubmissions.gameObject.SetActive(false);
        roundText.text = "Round " + Tournament.currentRound.index + "\n" + Tournament.currentRound.Bounty + " MTX";
        UpdateActionState();
    }

    public void ProcessMySubmissions(object result)
    {
        var mySubmissions = (List<MatryxSubmission>)result;
        // Avoid tournament confusion from varying callback timeouts
        if (mySubmissions.Count > 0 && 
            Tournament.address != mySubmissions[0].tournament.address)
        {
            return;
        }

        var tournamentAddress = Tournament.address;

        if (mySubmissions.Count == 0)
        {
            if (getKindOfSubmissionLastProcessed(tournamentAddress) == 0)
            {
                loadingSubmissions.text = "No submissions to display";
            }
        }
        else
        {
            loadingSubmissions.gameObject.SetActive(false);
            DisplaySubmissions((List<MatryxSubmission>)result, "My Submissions");
        }

        setSubmissionKindProcessed(tournamentAddress, 2);
    }

    public void ErrorLoadingTournament(object result)
    {
        loadingSubmissions.text = "Something went wrong. :(";
        descriptionText.text = "This would normally be the description of the tournament you just pulled up, but right now it's just filler because something has gone wrong. Try reloading this tournament?";
    }

    private void DisplayTournamentInfo()
    {
        bountyText.text = Tournament.Bounty + " MTX";
        timeRemainingText.text = Tournament.currentRound.StatusText;
        titleText.text = Tournament.title;
        creatorText.text = "by " + Utils.Accounts.ellipseAddress(Tournament.owner);
        roundText.text = "Round " + Tournament.currentRound.index + "\n" + Tournament.currentRound.Bounty + " MTX";

        descriptionText.text = Tournament.description;
    }

    public void DisplaySubmissionUI(MatryxSubmission submission)
    {
        submissionMenu.gameObject.GetComponent<AnimationHandler>().OpenMenu();
        submissionMenu.SetSubmission(submission);
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
        button.transform.position = new Vector3(-500f, -500f, -500f);
        button.GetComponent<SubmissionContainer>().submission = submission;

        var text = button.transform.Find("Text").GetComponent<TMPro.TextMeshPro>();
        button.name = submission.title;
        text.text = submission.title;

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
        Destroy(button.GetComponent<FlexRayCastHighlight>());
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

        var text = button.transform.Find("Text").GetComponent<TMPro.TextMeshPro>();
        text.text = "Reload Tournaments";
        text.fontStyle = TMPro.FontStyles.Bold;
        text.alignment = TMPro.TextAlignmentOptions.Center;

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

    private void Awake()
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
