using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class MyTournamentsButton : QuickButton
{
    [SerializeField]
    private MyTournamentsMenu myTournamentsMenu;
    [SerializeField]
    private FlexButtonComponent flexButtonComponent;

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

    public static MyTournamentsButton Instance { get; private set; }

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
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
        flexButtonComponent.SetState(1);
    }

    public void ToggleOff()
    {
        toggled = false;
        flexButtonComponent.selectedColor = ToggleOffColor;
        flexButtonComponent.passiveColor = LightPassiveColor;
        flexButtonComponent.hoveringColor = LightHoveringColor;

        flexButtonComponent.SetState(1);
    }

    public void ToggleOn()
    {
        toggled = true;
        myTournamentsMenu.GetComponent<AnimationHandler>().OpenMenu((obj) => { myTournamentsMenu.LoadMyTournaments(0); });
        flexButtonComponent.selectedColor = ToggleOnColor;
        flexButtonComponent.passiveColor = DarkPassiveColor;
        flexButtonComponent.hoveringColor = DarkHoveringColor;
        flexButtonComponent.SetState(2);
    }

    public override void OnMenuClose()
    {
        ToggleOff();
    }
}
