using UnityEngine;

public class CreateSubmissionButton : QuickButton {

    [SerializeField]
    private CreateSubmissionMenu canvasSubmissionMenu;
    [SerializeField]
    private FlexButtonComponent submissionButtonFlexComponent;


    private Color TOGGLE_ON = new Color(83f / 255f, 198f / 255f, 236f / 255f);
    private Color TOGGLE_OFF = new Color(117f / 255f, 205f / 255f, 234f / 255f);
    private Color DARK_PASSIVE = new Color(0.2f, 0.475f, 0.565f);
    private Color LIGHT_PASSIVE = Color.white;
    private Color DARK_HOVERING = new Color(87f / 255f, 178f / 255f, 208f / 255f);
    private Color LIGHT_HOVERING = new Color(132f / 255f, 223f / 255f, 253f / 255f); 

    protected override void ButtonEnterBehavior(GameObject other)
    {
        bool menuActive = canvasSubmissionMenu.gameObject.activeSelf;
        canvasSubmissionMenu.gameObject.SetActive(!menuActive);
        
        if(!menuActive)
        {
            submissionButtonFlexComponent.selectedColor = TOGGLE_ON;

            submissionButtonFlexComponent.passiveColor = DARK_PASSIVE;
            submissionButtonFlexComponent.hoveringColor = DARK_HOVERING;
            
        }
        else
        {
            submissionButtonFlexComponent.selectedColor = TOGGLE_OFF;

            submissionButtonFlexComponent.passiveColor = LIGHT_PASSIVE;
            submissionButtonFlexComponent.hoveringColor = LIGHT_HOVERING;
        }

        submissionButtonFlexComponent.SetState(2);
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
        submissionButtonFlexComponent.SetState(1);
    }
}
