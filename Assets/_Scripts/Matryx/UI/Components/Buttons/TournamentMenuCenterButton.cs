using UnityEngine;
using TMPro;
using Matryx;
using System;

public class TournamentMenuCenterButton : QuickButton {
    [SerializeField]
    TMPro.TextMeshPro text;
    [SerializeField]
    private CreateSubmissionMenu canvasSubmitMenu;
    [SerializeField]
    private FlexButtonComponent button;

    private Color TOGGLE_ON = new Color(83f / 255f, 198f / 255f, 236f / 255f);
    private Color TOGGLE_OFF = new Color(117f / 255f, 205f / 255f, 234f / 255f);
    private Color DARK_PASSIVE = new Color(0.2f, 0.475f, 0.565f);
    private Color LIGHT_PASSIVE = Color.white;
    private Color DARK_HOVERING = new Color(87f / 255f, 178f / 255f, 208f / 255f);
    private Color LIGHT_HOVERING = new Color(132f / 255f, 223f / 255f, 253f / 255f);

    private float defaultFontSize = 1.6f;
    private float otherFontSize = 1.3f;

    private bool toggled = false;
    public bool Toggled { get { return toggled; } }

    Action toggleOnAction;
    Action toggleOffAction;

    public static TournamentMenuCenterButton Instance { get; private set; }

    public void Awake()
    {
        if(Instance == null)
        {
            Instance = this;
        }
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        bool menuActive = canvasSubmitMenu.gameObject.activeSelf;
        canvasSubmitMenu.gameObject.SetActive(!menuActive);

        if (toggled)
        {
            ToggleOff();
        }
        else
        {
            ToggleOn();
        }

        // Select the button
        button.SetState(2);
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
        // Deselect the button
        button.SetState(1);
    }

    public void ToggleOff()
    {
        toggled = false;
        button.selectedColor = TOGGLE_OFF;
        button.passiveColor = LIGHT_PASSIVE;
        button.hoveringColor = LIGHT_HOVERING;

        text.text = "Contribute";
        text.fontSize = defaultFontSize;
        text.color = Color.black;

        button.SetState(1);

        switch (TournamentMenu.Instance.actionState)
        {
            case TournamentMenu.ActionState.NoAction:
                break;
            case TournamentMenu.ActionState.Contribute:
                Tippies.FadeDestroyTippy("Please Lift Headset");
                break;
            case TournamentMenu.ActionState.SelectWinners:
                TournamentMenu.Instance.submissionsPanel.MultiSelect = false;
                break;
            case TournamentMenu.ActionState.ManageTournament:
                // Close this window: Close Tournament --or-- Start New Round (fun screen. get here!)
                break;
        }
    }

    public void ToggleOn()
    {
        toggled = true;
        button.selectedColor = TOGGLE_ON;
        button.passiveColor = DARK_PASSIVE;
        button.hoveringColor = DARK_HOVERING;

        text.text = "Lift Headset...";
        text.fontSize = otherFontSize;
        text.color = Color.white;

        button.SetState(1);

        // Do what the 
        switch (TournamentMenu.Instance.actionState)
        {
            case TournamentMenu.ActionState.NoAction:
                break;
            case TournamentMenu.ActionState.Contribute:
                Tippies.SpawnTippy("Please Lift Headset", 4f, TMPro.TextAlignmentOptions.Center, new Vector2(6, 1f), 15f, AvatarSelector.centerEye, new Vector3(0f, 0f, 0.4f), 0.5f, 0.5f, Tippy.MovementMode.Soft, true);
                break;
            case TournamentMenu.ActionState.SelectWinners:
                // make tournament panel multi select
                TournamentMenu.Instance.submissionsPanel.SwitchToMultiSelect();
                // make sure clicking on one doesn't open up the submission (maybe use state dependency on something)
                // (check)
                // if at least one selected, create a button that allows you to Close Tournament --or-- Start New Round (radio)
                // (check)
                // Amend submission cell to contain a button that can be gripped and changed for distribution setting
                // (check)
                // every time a submission is selected, enable its distribution picker
                // (check)
                // continue button goes to same screen as Close Tournament --or-- Start New Round (radio), replace Close Tournament button with Do Nothing (radio)
                // ...
                // (validate the inputs for the new round: start time, endtime, bounty)
                // last button closes the window and fires select winners with the appropriate inputs based on what the user selected
                // in-VR transaction notification complete notifiction would be nice (Tippy? :) )
                break;
            case TournamentMenu.ActionState.ManageTournament:
                // Close Tournament --or-- Start New Round (fun screen. get here!)
                break;
        }
    }

    public void updateState()
    {
        switch (TournamentMenu.Instance.actionState)
        {
            case TournamentMenu.ActionState.NoAction:
                text.text = "Contribute";
                transform.parent.gameObject.SetActive(false);
                break;
            case TournamentMenu.ActionState.Contribute:
                transform.parent.gameObject.SetActive(true);
                button.SetState(0);
                text.text = "Contribute";
                break;
            case TournamentMenu.ActionState.SelectWinners:
                transform.parent.gameObject.SetActive(true);
                button.SetState(0);
                text.text = "Select Winners";
                break;
            case TournamentMenu.ActionState.ManageTournament:
                transform.parent.gameObject.SetActive(true);
                button.SetState(0);
                text.text = "Manage";
                break;
        }
    }
}
