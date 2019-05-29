using UnityEngine;
using Matryx;
using Color = UnityEngine.Color;

public class MyAccountButton : QuickButton
{
    [SerializeField]
    private MyAccountMenu accountMenu;
    [SerializeField]
    private FlexButtonComponent flexButtonComponent;
    [SerializeField]
    private SpriteRenderer iconRenderer;
    private Sprite iconSprite;

    bool blockieOnEnable = true;

    private Color PlusButtonColor = new Color((float)0x09 / (float)0xff, (float)0x3A / (float)0xff, (float)0x2C / (float)0xff);
    private Color ToggleOnColor = new Color(83f / 255f, 198f / 255f, 236f / 255f);
    private Color ToggleOffColor = new Color(117f / 255f, 205f / 255f, 234f / 255f);
    private Color DarkPassiveColor = new Color(0.2f, 0.475f, 0.565f);
    private Color LightPassiveColor = Color.white;
    private Color DarkHoveringColor = new Color(87f / 255f, 178f / 255f, 208f / 255f);
    private Color LightHoveringColor = new Color(132f / 255f, 223f / 255f, 253f / 255f);

    private float defaultFontSize = 1.4f;
    private float otherFontSize = 1.3f;
    Sprite defaultSprite;
    Sprite liftHeadsetSprite;

    public static MyAccountButton Instance { get; private set; }

    public void Awake()
    {
        if (Instance == null)
        {
            Instance = this;
        }
    }

    private void OnEnable()
    {
        if (blockieOnEnable)
        {
            updateBlockie();
        }
    }

    public void updateBlockie()
    {
        Texture2D blockieTex = Utils.Accounts.getBlockieTexture(NetworkSettings.currentAddress);
        var rect = new Rect(0f, 0f, blockieTex.width, blockieTex.height);
        var pivot = iconRenderer.sprite.pivot;
        iconSprite = Sprite.Create(blockieTex, rect, new Vector2(0.5f, 0.5f));
        iconRenderer.sprite = iconSprite;
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        Press();
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
        flexButtonComponent.SetState(1);
    }

    public void Press()
    {
        flexButtonComponent.SetState(2);
        accountMenu.Refresh();
        accountMenu.GetComponent<AnimationHandler>().OpenMenu();
    }
}
