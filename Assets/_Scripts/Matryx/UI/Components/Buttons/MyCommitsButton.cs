using UnityEngine;
using TMPro;
using Matryx;

public class MyCommitsButton : QuickButton
{
    [SerializeField]
    Renderer iconRenderer;
    [SerializeField]
    private MyCommitsMenu myCommitsMenu;
    [SerializeField]
    private FlexButtonComponent buttonFlexComponent;

    private Color PlusButtonColor = new Color((float)0x09 / (float)0xff, (float)0x3A / (float)0xff, (float)0x2C / (float)0xff);
    private Color ToggleOnColor = new Color(83f / 255f, 198f / 255f, 236f / 255f);
    private Color ToggleOffColor = new Color(117f / 255f, 205f / 255f, 234f / 255f);
    private Color DarkPassiveColor = new Color(0.2f, 0.475f, 0.565f);
    private Color LightPassiveColor = Color.white;
    private Color DarkHoveringColor = new Color(87f / 255f, 178f / 255f, 208f / 255f);
    private Color LightHoveringColor = new Color(132f / 255f, 223f / 255f, 253f / 255f);

    private float defaultFontSize = 1.4f;
    private float otherFontSize = 1.3f;

    private bool toggled = false;

    public static MyCommitsButton Instance { get; private set; }

    public void Awake()
    {
        if (Instance == null)
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

        buttonFlexComponent.SetState(2);
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
        buttonFlexComponent.SetState(1);
    }

    public void ToggleOff()
    {
        toggled = false;
        myCommitsMenu.GetComponent<AnimationHandler>().CloseMenu();
        buttonFlexComponent.selectedColor = ToggleOffColor;
        buttonFlexComponent.passiveColor = LightPassiveColor;
        buttonFlexComponent.hoveringColor = LightHoveringColor;

        buttonFlexComponent.SetState(1);
    }

    public void ToggleOn()
    {
        toggled = true;
        myCommitsMenu.GetComponent<AnimationHandler>().OpenMenu( (obj) => { myCommitsMenu.LoadMyCommits(0); });
        buttonFlexComponent.selectedColor = ToggleOnColor;
        buttonFlexComponent.passiveColor = DarkPassiveColor;
        buttonFlexComponent.hoveringColor = DarkHoveringColor;
        buttonFlexComponent.SetState(1);
    }

    public override void OnMenuClose()
    {
        ToggleOff();
    }
}
