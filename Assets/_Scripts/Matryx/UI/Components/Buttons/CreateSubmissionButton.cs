using UnityEngine;
using TMPro;

public class CreateSubmissionButton : QuickButton {

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

    public static CreateSubmissionButton Instance { get; private set; }

    public void Awake()
    {
        if(Instance == null)
        {
            Instance = this;
        }
    }

    public void ReturnFromSubmit()
    {
        // Adjust button colors for closing submit menu
        button.selectedColor = TOGGLE_OFF;
        button.passiveColor = LIGHT_PASSIVE;
        button.hoveringColor = LIGHT_HOVERING;

        button.SetState(1);
        ToggleOff();

        // Hide remove headset label on wrist
        //congratulationsMessage.gameObject.SetActive(false);
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
    }
}
