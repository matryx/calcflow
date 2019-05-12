using Calcflow.UserStatistics;
using Matryx;
using Nanome.Core;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using UnityEngine;
using Vector3 = UnityEngine.Vector3;

public class ManageTournamentMenu : MenuStateReceiver {

    public static ManageTournamentMenu Instance { get; private set; }
    public static List<MatryxSubmission> winningSubmissions = new List<MatryxSubmission>();
    public static List<BigInteger> distribution = new List<BigInteger>();

    [SerializeField]
    NewRoundMenu newRoundMenu;
    FlexMenu flexMenu;
    FlexPanelComponent mainPanel;
    Dictionary<string, FlexButtonComponent> buttons = new Dictionary<string, FlexButtonComponent>();
    Dictionary<string, TMPro.TextMeshPro> text = new Dictionary<string, TMPro.TextMeshPro>();
    [SerializeField]
    TMPro.TextMeshPro middleTextOne;
    [SerializeField]
    GameObject winnerSpinner;
    [SerializeField]
    GameObject winnerCheck;
    [SerializeField]
    GameObject closeSpinner;
    [SerializeField]
    GameObject closeCheck;

    internal class ManageTournamentMenuResponder : FlexMenu.FlexMenuResponder
    {
        ManageTournamentMenu manageMenu;
        internal ManageTournamentMenuResponder(ManageTournamentMenu manageMenu)
        {
            this.manageMenu = manageMenu;
        }

        public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
        {
            manageMenu.HandleInput(sender.gameObject);
        }

        public void Flex_ActionEnd(string name, FlexActionableComponent sender, GameObject collider)
        {
            sender.SetState(0);
        }
    }

    bool winnersToggled = false;
    bool closeToggled = false;
    bool roundToggled = false;
    public void HandleInput(GameObject sender)
    {
        switch (sender.name)
        {
            case "WinnersOnlyButton":
                ToggleButton("WinnersYes");
                ToggleButton("WinnersNo");
                ToggleText("Are You Sure Winners");
                winnersToggled = !winnersToggled;
                ToggleFlexButton(winnersToggled, buttons["WinnersOnlyButton"]);
                break;
            case "NewRoundButton":
                roundToggled = !roundToggled;
                string btnText = "Create New\nRound";
                if (roundToggled)
                {
                    btnText = "Please Lift\nHeadset";
                    Tippies.SpawnTippy("Please Lift Headset", 4f, TMPro.TextAlignmentOptions.Center, new Vector3(1f, 0.25f, 0.05f), 15f, AvatarSelector.centerEye, new Vector3(0f, 0f, 0.4f), 0.5f, 0.5f, Tippy.MovementMode.Soft, true);
                    newRoundMenu.gameObject.SetActive(true);
                    newRoundMenu.SetTournament(TournamentMenu.Tournament);
                }
                else
                {
                    Tippies.DestroyTippy("Please Lift Headset");
                    newRoundMenu.gameObject.SetActive(false);
                }
                ToggleFlexButton(roundToggled, buttons["NewRoundButton"], btnText);
                break;
            case "CloseTournamentButton":
                ToggleButton("CloseTournamentYes");
                ToggleButton("CloseTournamentNo");
                ToggleText("Are You Sure Tournament");
                closeToggled = !closeToggled;
                ToggleFlexButton(closeToggled, buttons["CloseTournamentButton"]);
                break;
            case "WinnersYes":
                if (!winSpin)
                {
                    SpinForTheWin();
                    SetButtonsEnabled(false);
                    FireSelectWinners();
                }
                break;
            case "WinnersNo":
                HandleInput(buttons["WinnersOnlyButton"].gameObject);
                break;
            case "CloseTournamentYes":
                SpinForTheClose();
                SetButtonsEnabled(false);
                FireCloseTournament();
                break;
            case "CloseTournamentNo":
                HandleInput(buttons["CloseTournamentButton"].gameObject);
                break;
            default:
                break;
        }
    }

    public override void OnMenuOpen()
    {
        List<string> submissions = new List<string>(TournamentMenu.Instance.submissionsPanel.selected.Keys);
        List<GameObject> submissionCells = new List<GameObject>(TournamentMenu.Instance.submissionsPanel.selected.Values);

        for (int i = 0; i < submissionCells.Count; i++)
        {
            var submission = submissionCells[i].GetComponent<SubmissionContainer>().submission;
            winningSubmissions.Add(submission);
            distribution.Add(new BigInteger(DistributionPicker.rewards[submission]));
        }
    }

    private void ToggleButton(string name)
    {
        var btn = buttons[name].gameObject;
        btn.SetActive(!btn.activeSelf);
    }

    private void ToggleText(string name)
    {
        var txt = text[name].gameObject;
        txt.SetActive(!txt.activeSelf);
    }

    private void ToggleFlexButton(bool toggle, FlexButtonComponent sender, string newText = null)
    {
        var button = sender.GetComponent<FlexButtonComponent>();
        var text = button.GetComponentInChildren<TMPro.TextMeshPro>();
        if (newText != null) { text.text = newText; }

        if (toggle)
        {
            button.selectedColor = QuickButton.TOGGLE_ON;
            button.passiveColor = QuickButton.DARK_PASSIVE;
            button.hoveringColor = QuickButton.DARK_HOVERING;
            text.color = Color.white;

            button.SetState(2);
        }
        else
        {
            button.selectedColor = QuickButton.TOGGLE_OFF;
            button.passiveColor = QuickButton.LIGHT_PASSIVE;
            button.hoveringColor = QuickButton.LIGHT_HOVERING;
            text.color = Color.black;

            button.SetState(1);
            button.SetState(0);
        }
    }

    public void PressButton(string name, string newText = null)
    {
        buttons[name].SetState(-1);
        buttons[name].SetState(0);
        HandleInput(buttons[name].gameObject);
    }

    public static void SetButtonsEnabled(bool enabled)
    {
        var state = enabled ? 0 : -1;
        foreach(KeyValuePair<string,FlexButtonComponent> pair in Instance.buttons)
        {
            if (pair.Key.Equals("CloseButton")) continue;
            pair.Value.SetState(state);
        }
    }

    void Start () {
        Initialize();
	}

    bool winSpin;
    bool closeSpin;
    Vector3 dSpin = new Vector3(0, 0, -12f);
    void FixedUpdate()
    {
        if (winSpin)
        {
            winnerSpinner.transform.Rotate(dSpin);
        }

        if(closeSpin)
        {
            closeSpinner.transform.Rotate(dSpin);
        }
    }

    public void SpinForTheWin(bool spin=true)
    {
        winSpin = spin;
        winnerSpinner.SetActive(spin);
        winnerCheck.SetActive(!spin);
    }

    public void SpinForTheClose(bool spin=true)
    {
        closeSpin = spin;
        closeSpinner.SetActive(spin);
        closeCheck.SetActive(!spin);
    }

    bool initialized = false;
    private void Initialize()
    {
        if (!initialized)
        {
            initialized = true;
            Instance = this;

            ManageTournamentMenuResponder responder = new ManageTournamentMenuResponder(this);
            flexMenu = GetComponent<FlexMenu>();
            mainPanel = GetComponentInChildren<FlexPanelComponent>();
            var btns = GetComponentsInChildren<FlexButtonComponent>(true);
            foreach (var button in btns)
            {
                mainPanel.AddAction(button);
                buttons.Add(button.gameObject.name, button);
            }
            var txts = GetComponentsInChildren<TMPro.TextMeshPro>(true);
            foreach (var txt in txts)
            {
                if(!text.ContainsKey(txt.gameObject.name))
                {
                    text.Add(txt.gameObject.name, txt);
                }
            }

            flexMenu.RegisterResponder(responder);
        }
    }

    public void UpdateState()
    {
        if (TournamentMenu.Tournament.currentRound.winningSubmissions.Count > 0)
        {
            buttons["WinnersOnlyButton"].gameObject.SetActive(false);
            middleTextOne.gameObject.SetActive(false);
        }
        else
        {
            buttons["WinnersOnlyButton"].gameObject.SetActive(true);
            middleTextOne.gameObject.SetActive(true);
        }
    }

    public void FireSelectWinners()
    {
        Async.runInCoroutine(delegate (Async thread, object param)
        {
            var submissions = winningSubmissions.Select(sub => Utils.HexStringToByteArray(sub.hash)).ToList();
            return TournamentMenu.Tournament.selectWinners(submissions, distribution, new BigInteger((int)MatryxTournament.SelectWinnerAction.DoNothing), BigInteger.Zero, BigInteger.Zero, BigInteger.Zero, BigInteger.Zero,
            delegate (object result)
            {
                Debug.Log((bool) result ? "Successfully selected winners!" : "Winner selection unsuccessful.");
                SpinForTheWin(false);
                PressButton("WinnersOnlyButton");
                if ((bool) result)
                {
                    StatisticsTracking.EndEvent("Matryx", "Winner Selection Only", new Dictionary<string, object>() { { "success", true } });
                    Tippies.SpawnTippy("Winner Selection Transaction Successful", 4f, TMPro.TextAlignmentOptions.Center, new Vector3(1f, 0.25f, 0.05f), 15f, AvatarSelector.centerEye, new Vector3(0f, 0f, 0.4f), 0.5f, 0.5f, Tippy.MovementMode.Soft, true);
                    TournamentMenu.Tournament.currentRound.winningSubmissions = winningSubmissions;
                    TournamentMenu.Instance.ProcessRound(TournamentMenu.Tournament.currentRound);
                    
                }
                else
                {
                    StatisticsTracking.EndEvent("Matryx", "Winner Selection Only", new Dictionary<string, object>() { { "success", false } });
                    Tippies.SpawnTippy("Winner Selection Transaction Failed", 4f, TMPro.TextAlignmentOptions.Center, new Vector3(1f, 0.25f, 0.05f), 15f, AvatarSelector.centerEye, new Vector3(0f, 0f, 0.4f), 0.5f, 0.5f, Tippy.MovementMode.Soft, true);
                }
            });
        });
    }

    public void FireCloseTournament()
    {
        if (TournamentMenu.Tournament.currentRound.winningSubmissions.Count == 0)
        {
            Async.runInCoroutine(delegate (Async thread, object param)
            {
                var submissions = winningSubmissions.Select(sub => Utils.HexStringToByteArray(sub.hash)).ToList();
                return TournamentMenu.Tournament.selectWinners(submissions, distribution, new BigInteger((int)MatryxTournament.SelectWinnerAction.CloseTournament), BigInteger.Zero, BigInteger.Zero, BigInteger.Zero, BigInteger.Zero,
                delegate (object result)
                {
                    Debug.Log((bool)result ? "Successfully closed the tournament!" : "Close tournament unsuccessful.");
                    SpinForTheClose(false);
                    PressButton("CloseTournamentButton");
                    if ((bool)result)
                    {
                        StatisticsTracking.EndEvent("Matryx", "Winner Selection & Tournament Close", new Dictionary<string, object>() { { "success", true } });
                        Tippies.SpawnTippy("Close Tournament Transaction Successful", 4f, TMPro.TextAlignmentOptions.Center, new Vector3(1f, 0.25f, 0.05f), 15f, AvatarSelector.centerEye, new Vector3(0f, 0f, 0.4f), 0.5f, 0.5f, Tippy.MovementMode.Soft, true);
                        buttons["CloseButton"].transform.Find("Body").GetComponent<RayCastButton>().PressButton(null);
                    }
                    else
                    {
                        StatisticsTracking.EndEvent("Matryx", "Winner Selection Only", new Dictionary<string, object>() { { "success", false } });
                        Tippies.SpawnTippy("Close Tournament Transaction Failed", 4f, TMPro.TextAlignmentOptions.Center, new Vector3(1f, 0.25f, 0.05f), 15f, AvatarSelector.centerEye, new Vector3(0f, 0f, 0.4f), 0.5f, 0.5f, Tippy.MovementMode.Soft, true);
                    }
                });
            });
        }
        else
        {
            Async.runInCoroutine(delegate (Async thread, object param)
            {
                var submissions = winningSubmissions.Select(sub => Utils.HexStringToByteArray(sub.hash)).ToList();
                return TournamentMenu.Tournament.closeTournament(
                delegate (object result)
                {
                    Debug.Log((bool)result ? "Successfully closed the tournament!" : "Close tournament unsuccessful.");
                    SetButtonsEnabled(true);
                    SpinForTheClose(false);
                    if ((bool)result)
                    {
                        StatisticsTracking.EndEvent("Matryx", "Tournament Close Only", new Dictionary<string, object>() { { "success", true } });
                        Tippies.SpawnTippy("Close Tournament Transaction Successful", 4f, TMPro.TextAlignmentOptions.Center, new Vector3(1f, 0.25f, 0.05f), 15f, AvatarSelector.centerEye, new Vector3(0f, 0f, 0.4f), 0.5f, 0.5f, Tippy.MovementMode.Soft, true);
                        buttons["CloseButton"].transform.Find("Body").GetComponent<RayCastButton>().PressButton(null);
                    }
                    else
                    {
                        StatisticsTracking.EndEvent("Matryx", "Tournament Close Only", new Dictionary<string, object>() { { "success", false } });
                        Tippies.SpawnTippy("Close Tournament Transaction Failed", 4f, TMPro.TextAlignmentOptions.Center, new Vector3(1f, 0.25f, 0.05f), 15f, AvatarSelector.centerEye, new Vector3(0f, 0f, 0.4f), 0.5f, 0.5f, Tippy.MovementMode.Soft, true);
                    }
                });
            });
        }
        
    }
}
