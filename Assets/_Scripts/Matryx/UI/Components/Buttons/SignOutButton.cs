using UnityEngine;
using TMPro;
using Matryx;

public class SignOutButton : QuickButton
{
    [SerializeField]
    private MyAccountMenu accountMenu;
    [SerializeField]
    private FlexButtonComponent flexButtonComponent;
    [SerializeField]
    private TMPro.TextMeshPro labelText;

    private Color SelectedColor = new Color(0xC2, 0xF0, 0xFF);

    private float defaultFontSize = 1.4f;
    private float otherFontSize = 1.3f;
    Sprite defaultSprite;
    Sprite liftHeadsetSprite;

    public static SignOutButton Instance { get; private set; }

    public void Awake()
    {
        if (Instance == null)
        {
            Instance = this;
            flexButtonComponent.selectedColor = Color.white;
        }
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        flexButtonComponent.SetState(2);
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
        Press();
        flexButtonComponent.SetState(1);
    }

    public void Press()
    {
        flexButtonComponent.SetState(2);
        accountMenu.ClearAccounts();
        // Actually sign out
        NetworkSettings.SignOut();
        accountMenu.GetComponent<AnimationHandler>().CloseMenu();
    }
}
