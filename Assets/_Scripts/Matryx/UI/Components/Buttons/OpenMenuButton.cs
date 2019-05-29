using UnityEngine;
using TMPro;
using Matryx;

public class OpenMenuButton : QuickButton
{
    [SerializeField]
    private GameObject menu;
    private FlexButtonComponent flexButton;
    private Color SelectedColor = new Color(0xC2, 0xF0, 0xFF);

    public void Awake()
    {
        flexButton = GetComponent<FlexButtonComponent>();
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        flexButton.SetState(2);
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
        menu.GetComponent<AnimationHandler>().OpenMenu();
        menu.GetComponent<MenuStateReceiver>().OnMenuOpen();
        flexButton.SetState(1);
    }
}