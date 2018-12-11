using UnityEngine;
using TMPro;

public class CreateSubmissionButton : QuickButton {

    [SerializeField]
    TMPro.TextMeshPro text;
    [SerializeField]
    private SubmitMenu canvasSubmitMenu;
    [SerializeField]
    private FlexButtonComponent submissionButtonFlexComponent;

    private Color TOGGLE_ON = new Color(83f / 255f, 198f / 255f, 236f / 255f);
    private Color TOGGLE_OFF = new Color(117f / 255f, 205f / 255f, 234f / 255f);
    private Color DARK_PASSIVE = new Color(0.2f, 0.475f, 0.565f);
    private Color LIGHT_PASSIVE = Color.white;
    private Color DARK_HOVERING = new Color(87f / 255f, 178f / 255f, 208f / 255f);
    private Color LIGHT_HOVERING = new Color(132f / 255f, 223f / 255f, 253f / 255f);

    private float defaultFontSize = 1.6f;
    private float otherFontSize = 1.3f;

    public void ReturnFromSubmit()
    {
        // Adjust button colors for closing submit menu
        submissionButtonFlexComponent.selectedColor = TOGGLE_OFF;
        submissionButtonFlexComponent.passiveColor = LIGHT_PASSIVE;
        submissionButtonFlexComponent.hoveringColor = LIGHT_HOVERING;

        submissionButtonFlexComponent.SetState(1);
        SetSubmitButtonToContribute();

        // Hide remove headset label on wrist
        //congratulationsMessage.gameObject.SetActive(false);
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        bool menuActive = canvasSubmitMenu.gameObject.activeSelf;
        canvasSubmitMenu.gameObject.SetActive(!menuActive);
        
        if(!menuActive)
        {
            // Adjust button colors for bringing up submit menu
            submissionButtonFlexComponent.selectedColor = TOGGLE_ON;
            submissionButtonFlexComponent.passiveColor = DARK_PASSIVE;
            submissionButtonFlexComponent.hoveringColor = DARK_HOVERING;

            // Display remove headset label on wrist
            SetSubmitButtonToLiftHeadset();
        }
        else
        {
            // Adjust button colors for closing submit menu
            submissionButtonFlexComponent.selectedColor = TOGGLE_OFF;
            submissionButtonFlexComponent.passiveColor = LIGHT_PASSIVE;
            submissionButtonFlexComponent.hoveringColor = LIGHT_HOVERING;

            // Hide remove headset label on wrist
            //congratulationsMessage.gameObject.SetActive(false);
            SetSubmitButtonToContribute();
        }

        // Select the button
        submissionButtonFlexComponent.SetState(2);
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
        // Deselect the button
        submissionButtonFlexComponent.SetState(1);
    }

    public void SetSubmitButtonToContribute()
    {
        text.text = "Contribute";
        text.fontSize = defaultFontSize;
        text.color = Color.black;
    }

    public void SetSubmitButtonToLiftHeadset()
    {
        text.text = "Lift Headset...";
        text.fontSize = otherFontSize;
        text.color = Color.white;
    }
}
