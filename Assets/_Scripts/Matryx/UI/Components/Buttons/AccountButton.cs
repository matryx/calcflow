using UnityEngine;
using TMPro;
using Matryx;

public class AccountButton : QuickButton
{
    [SerializeField]
    SpriteRenderer spriteRenderer;
    [SerializeField]
    private AccountMenu accountMenu;
    [SerializeField]
    private FlexButtonComponent flexButtonComponent;
    [SerializeField]
    private TMPro.TextMeshPro labelText;

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

    public static AccountButton Instance { get; private set; }

    public void Awake()
    {
        if (Instance == null)
        {
            Instance = this;
            defaultSprite = spriteRenderer.sprite;
            var texture = Resources.Load<Texture2D>("Icons/liftheadset_inverted_medium_ben");
            var rect = new Rect(0f, 0f, texture.width, texture.height);
            var pivot = spriteRenderer.sprite.pivot;
            liftHeadsetSprite = Sprite.Create(texture, rect, new Vector2(0.56f, 0.52f));
        }
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        bool menuActive = accountMenu.gameObject.activeSelf;
        accountMenu.gameObject.SetActive(!menuActive);

        Press();
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
        flexButtonComponent.SetState(1);
    }

    public void Press()
    {
        flexButtonComponent.SetState(2);

        //accountMenu.gameObject.
    }
}
