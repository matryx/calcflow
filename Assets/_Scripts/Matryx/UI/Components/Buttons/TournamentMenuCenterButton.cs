using UnityEngine;
using TMPro;
using Matryx;
using System;
using System.Collections.Generic;

public class TournamentMenuCenterButton : QuickButton {
    [SerializeField]
    TournamentMenu tournamentMenu;
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

        text.fontSize = defaultFontSize;
        text.color = Color.black;

        button.SetState(1);

        switch (tournamentMenu.actionState)
        {
            case TournamentMenu.ActionState.NoAction:
                break;
            case TournamentMenu.ActionState.Contribute:
                canvasSubmitMenu.gameObject.SetActive(false);
                text.text = "Contribute";
                Tippies.FadeDestroyTippy("Please Lift Headset");
                break;
            case TournamentMenu.ActionState.SelectWinners:
                text.text = "Select Winners";
                foreach (KeyValuePair<string,GameObject> pair in tournamentMenu.submissionsPanel.selected)
                {
                    var submissionContainer = pair.Value.GetComponent<SubmissionContainer>();
                    submissionContainer.distributionPicker.Toggle(false);
                }
                if(tournamentMenu == null || tournamentMenu.submissionsPanel == null)
                {
                    Debug.Log("here");
                }
                tournamentMenu.submissionsPanel.SwitchToSingleSelect();
                tournamentMenu.continueButton.gameObject.SetActive(false);
                break;
            case TournamentMenu.ActionState.ManageTournament:
                text.text = "Manage";
                TournamentMenu.Instance.manageTournamentMenu.GetComponent<AnimationHandler>().CloseMenu();
                break;
        }
    }

    public void ToggleOn()
    {
        switch (tournamentMenu.actionState)
        {
            case TournamentMenu.ActionState.NoAction:
                return;
            case TournamentMenu.ActionState.Contribute:
                canvasSubmitMenu.gameObject.SetActive(true);
                Tippies.SpawnTippy("Please Lift Headset", 4f, TMPro.TextAlignmentOptions.Center, new Vector3(1f, 0.25f, 0.05f), 15f, AvatarSelector.centerEye, new Vector3(0f, 0f, 0.4f), 0.5f, 0.5f, Tippy.MovementMode.Soft, true);
                break;
            case TournamentMenu.ActionState.SelectWinners:
                // make tournament panel multi select
                text.text = "Choose\nFrom Below...";
                tournamentMenu.submissionsPanel.SwitchToMultiSelect();
                if (tournamentMenu.submissionsPanel.selected.Count > 0)
                {
                    tournamentMenu.continueButton.gameObject.SetActive(true);
                }
                break;
            case TournamentMenu.ActionState.ManageTournament:
                text.text = "Managing...";
                TournamentMenu.Instance.manageTournamentMenu.UpdateState();
                TournamentMenu.Instance.manageTournamentMenu.GetComponent<AnimationHandler>().OpenMenu();
                break;
        }

        toggled = true;
        button.selectedColor = TOGGLE_ON;
        button.passiveColor = DARK_PASSIVE;
        button.hoveringColor = DARK_HOVERING;

        text.fontSize = otherFontSize;
        text.color = Color.white;

        button.SetState(1);
    }

    public void updateState()
    {
        switch (tournamentMenu.actionState)
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
    public void Enable()
    {
        transform.parent.gameObject.GetComponent<FlexButtonComponent>().SetState(0);
    }
    public void Disable()
    {
        transform.parent.gameObject.GetComponent<FlexButtonComponent>().SetState(-1);
    }

    public override void OnMenuClose()
    {
        ToggleOff();
    }
}
