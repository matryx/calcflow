using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

using Matryx;
using System.Numerics;
using Vector3 = UnityEngine.Vector3;

public class MySubmissionsMenu : MonoBehaviour
{
    public static MySubmissionsMenu Instance { get; private set; }
    public MatryxTournament tournament;

    private MultiSelectFlexPanel submissionsPanel;
    [SerializeField]
    private TMPro.TextMeshPro mySubmissionsText;
    [SerializeField]
    private TMPro.TextMeshPro loadingText;
    [SerializeField]
    private SubmissionMenu submissionMenu;
    

    internal class KeyboardInputResponder : FlexMenu.FlexMenuResponder
    {
        public FlexMenu menu;
        MySubmissionsMenu submissionsMenu;
        internal KeyboardInputResponder(MySubmissionsMenu submissionsMenu, FlexMenu menu)
        {
            this.menu = menu;
            this.submissionsMenu = submissionsMenu;
        }

        public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
        {
            submissionsMenu.HandleInput(sender.gameObject);
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
        scroll = GetComponentInChildren<Scroll>(true);
        flexMenu = GetComponent<FlexMenu>();
        KeyboardInputResponder responder = new KeyboardInputResponder(this, flexMenu);
        flexMenu.RegisterResponder(responder);
        submissionsPanel = GetComponentInChildren<MultiSelectFlexPanel>().Initialize();
        joyStickAggregator = scroll.GetComponent<JoyStickAggregator>();
    }
    
    public void LoadMySubmissions(MatryxTournament tournament)
    {
        Instance.tournament = tournament;
        loadingText.gameObject.SetActive(true);
        ClearSubmissions();
        MatryxExplorer.RunFetchMySubmissions(tournament, ProcessSubmissions);
    }

    /// <summary>
    /// Loads the next page of tournaments.
    /// </summary>
    public void LoadMoreMySubmissions()
    {
        LoadMySubmissions(tournament);
    }

    /// <summary>
    /// Clears the list of tournaments.
    /// </summary>
    public void ClearSubmissions()
    {
        scroll.clear();
    }

    private void ProcessSubmissions(object results)
    {
        loadingText.gameObject.SetActive(false);
        DisplaySubmissions((List<MatryxSubmission>)results);
    }

    GameObject loadButton;
    private void DisplaySubmissions(List<MatryxSubmission> submissions)
    {
        List<Transform> toAdd = new List<Transform>();
        foreach (MatryxSubmission submission in submissions)
        {
            GameObject button = createButton(submission);
            button.SetActive(false);
            submissionsPanel.AddAction(button.GetComponent<FlexButtonComponent>());
        }
    }
    
    private GameObject createButton(MatryxSubmission submission)
    {
        GameObject button = Instantiate(Resources.Load("Submission_Cell", typeof(GameObject))) as GameObject;
        button.transform.SetParent(submissionsPanel.transform);
        button.transform.localScale = Vector3.one;

        button.name = submission.details.title;
        button.GetComponent<SubmissionContainer>().SetSubmission(submission);

        var buttonText = button.transform.Find("Text").GetComponent<TMPro.TextMeshPro>();
        buttonText.text = submission.details.title;
        buttonText.alignment = TMPro.TextAlignmentOptions.Center;

        scroll.addObject(button.transform);
        joyStickAggregator.AddForwarder(button.GetComponentInChildren<JoyStickForwarder>());

        return button;
    }
    
    private void HandleInput(GameObject source)
    {
        if (source.name == "Load_Button")
        {
            LoadMoreMySubmissions();
        }
        else if (source.GetComponent<SubmissionContainer>() != null)
        {
            string name = source.name;

            MatryxSubmission submission = source.GetComponent<SubmissionContainer>().GetSubmission();
            StartCoroutine(submission.getFile());
            submissionMenu.SetSubmission(submission);
            // TODO: Navigate the user to the corresponding tournament through the menus
            submissionMenu.gameObject.GetComponent<AnimationHandler>().OpenMenu();
        }
    }
}
