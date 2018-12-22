using UnityEngine;
using TMPro;
using Matryx;

public class UnlockAccountButton : QuickButton
{

    [SerializeField]
    TMPro.TextMeshPro text;
    [SerializeField]
    private MatryxAccountMenu accountMenu;
    [SerializeField]
    private FlexButtonComponent submissionButtonFlexComponent;

    private Color TOGGLE_ON = new Color(83f / 255f, 198f / 255f, 236f / 255f);
    private Color TOGGLE_OFF = new Color(117f / 255f, 205f / 255f, 234f / 255f);
    private Color DARK_PASSIVE = new Color(0.2f, 0.475f, 0.565f);
    private Color LIGHT_PASSIVE = Color.white;
    private Color DARK_HOVERING = new Color(87f / 255f, 178f / 255f, 208f / 255f);
    private Color LIGHT_HOVERING = new Color(132f / 255f, 223f / 255f, 253f / 255f);

    private float defaultFontSize = 1.4f;
    private float otherFontSize = 1.3f;

    public void ReturnFromSubmit()
    {
        submissionButtonFlexComponent.selectedColor = TOGGLE_OFF;
        submissionButtonFlexComponent.passiveColor = LIGHT_PASSIVE;
        submissionButtonFlexComponent.hoveringColor = LIGHT_HOVERING;

        submissionButtonFlexComponent.SetState(1);
        SetButtonToUnlock();
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        bool menuActive = accountMenu.gameObject.activeSelf;

        if (!menuActive)
        {
            TournamentsMenu.SetState(TournamentsMenu.TournamentMenuState.WaitingForUnlock);
        }
        else
        {
            TournamentsMenu.SetState(TournamentsMenu.TournamentMenuState.AccountUnlockRequired);
        }

        accountMenu.gameObject.SetActive(!menuActive);

        // Select the button
        submissionButtonFlexComponent.SetState(2);
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
        // Deselect the button
        submissionButtonFlexComponent.SetState(1);
    }

    public void SetButtonToUnlock()
    {
        text.text = "Unlock Account";
        text.fontSize = defaultFontSize;
        text.color = Color.black;

        submissionButtonFlexComponent.selectedColor = TOGGLE_OFF;
        submissionButtonFlexComponent.passiveColor = LIGHT_PASSIVE;
        submissionButtonFlexComponent.hoveringColor = LIGHT_HOVERING;

        submissionButtonFlexComponent.SetState(1);
    }

    public void SetButtonToCancel()
    {
        text.text = "Cancel";
        text.fontSize = otherFontSize;
        text.color = Color.white;

        submissionButtonFlexComponent.selectedColor = TOGGLE_ON;
        submissionButtonFlexComponent.passiveColor = DARK_PASSIVE;
        submissionButtonFlexComponent.hoveringColor = DARK_HOVERING;

        submissionButtonFlexComponent.SetState(1);

    }
}
