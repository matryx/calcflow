using Matryx;
using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class MyCommitsMenu : MonoBehaviour {

    public MyCommitsMenu Instance { get; private set; }
    private MultiSelectFlexPanel commitsPanel;
    [SerializeField]
    CommitMenu commitMenu;
    [SerializeField]
    private TMPro.TextMeshPro commitsListText;

    private Scroll scroll;
    JoyStickAggregator joyStickAggregator;
    FlexMenu flexMenu;

    private Dictionary<string, MatryxCommit> commits = new Dictionary<string, MatryxCommit>();

    internal class CommitButtonResponder : FlexMenu.FlexMenuResponder
    {
        public FlexMenu menu;
        MyCommitsMenu myCommitsMenu;
        internal CommitButtonResponder(MyCommitsMenu myCommits, FlexMenu menu)
        {
            this.menu = menu;
            this.myCommitsMenu = myCommits;
        }

        public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
        {
            myCommitsMenu.HandleInput(sender.gameObject);
        }

        public void Flex_ActionEnd(string name, FlexActionableComponent sender, GameObject collider) { }
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
        commitsPanel = GetComponentInChildren<MultiSelectFlexPanel>().Initialize();
        joyStickAggregator = scroll.GetComponent<JoyStickAggregator>();
    }

    // Use this for initialization
    void Start () {
		
	}
	
	// Update is called once per frame
	void Update () {
		
	}

    public void ClearCommits()
    {
        page = 0;
        commits.Clear();
        scroll.clear();
    }

    int page = 0;
    bool loaded
    {
        get { return commits.Count != 0; }
    }
    bool loading = false;
    public bool LoadMyCommits(int thePage)
    {
        if ((loaded | loading) == true) return false;
        loading = true;
        commitsListText.text = "Loading Commits...";
        ClearCommits();
        MatryxCortex.RunGetMyCommits(ProcessCommits, ShowError);
        return true;
    }

    /// <summary>
    /// Loads the next page of tournaments.
    /// </summary>
    public void LoadMoreMyCommits()
    {
        loading = false;
        if (LoadMyCommits(page + 1))
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
        commits.Clear();
        scroll.clear();
    }

    private void ProcessCommits(object results)
    {
        var resultingCommits = (List<MatryxCommit>)results;
        commitsListText.text = "My Creations";
        DisplayCommits(resultingCommits);
    }

    private void ShowError(object results)
    {
        commitsListText.text = "Unable to Load Your Creations";
    }

    GameObject loadButton;
    private void DisplayCommits(List<MatryxCommit> _commits)
    {
        foreach (MatryxCommit commit in _commits)
        {
            commits.Add(commit.hash, commit);
            GameObject button = createButton(commit);
            button.SetActive(false);
            commitsPanel.AddAction(button.GetComponent<FlexButtonComponent>());
        }

        loadButton = createLoadButton();
    }

    private GameObject createLoadButton()
    {
        GameObject button = Instantiate(Resources.Load("Commit_Cell", typeof(GameObject))) as GameObject;
        button.transform.SetParent(commitsPanel.transform);
        button.transform.localScale = Vector3.one;

        button.name = "Load_Button";
        var textMesh = button.transform.Find("Text").GetComponent<TMPro.TextMeshPro>();
        textMesh.text = "(Reload Commits)";
        textMesh.fontStyle = TMPro.FontStyles.Bold;

        var icon = button.transform.Find("Icon");
        var notAvailable = button.transform.Find("PreviewNotAvailable");
        icon.gameObject.SetActive(false);
        notAvailable.gameObject.SetActive(false);

        scroll.addObject(button.transform);
        joyStickAggregator.AddForwarder(button.GetComponentInChildren<JoyStickForwarder>());

        // Add this action to the panel
        commitsPanel.AddAction(button.GetComponent<FlexButtonComponent>());

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

    private GameObject createButton(MatryxCommit commit)
    {
        GameObject button = Instantiate(Resources.Load("Commit_Cell", typeof(GameObject))) as GameObject;
        button.transform.SetParent(commitsPanel.transform);
        button.transform.localScale = Vector3.one;

        var commitTimestamp = Utils.Time.FromUnixTime(long.Parse(commit.timestamp.ToString()));
        button.name = commitTimestamp.ToLongDateString();
        button.GetComponent<CommitContainer>().commit = commit;

        button.transform.Find("Icon").GetComponent<Renderer>().material.mainTexture = commit.previewImage;
        button.transform.Find("Text").GetComponent<TMPro.TextMeshPro>().text = button.name;

        scroll.addObject(button.transform);
        joyStickAggregator.AddForwarder(button.GetComponentInChildren<JoyStickForwarder>());

        return button;
    }


    private void HandleInput(GameObject source)
    {
        if (source.name == "Load_Button")
        {
            LoadMoreMyCommits();
        }
        else if (source.GetComponent<CommitContainer>())
        {
            string name = source.name;

            MatryxCommit commit = source.GetComponent<CommitContainer>().commit;
            // TODO: Navigate the user to the corresponding tournament through the menus
            commitMenu.gameObject.GetComponent<AnimationHandler>().OpenMenu();
            commitMenu.SetCommit(commit);
        }
    }
}
