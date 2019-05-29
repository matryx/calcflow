using UnityEngine;

public class ClaimCommitButton : FlexHoverTipper
{
    public static ClaimCommitButton Instance { get; private set; }
    CommitButtonState state;
    public Matryx.MatryxCommit commit;

    bool spin = false;
    Vector3 dSpin;
    GameObject logo;
    GameObject spinner;

    FlexButtonComponent flexButton;

    Vector3 stampPosition;
    Vector3 matryxPosition;

    Sprite stampSprite;
    Sprite matryxSprite;

    // Use this for initialization
    private new void Start()
    {
        base.Start();
        if (Instance == null) { Instance = this; }

        fontSize = 1.1f;
        alignment = TMPro.TextAlignmentOptions.Center;
        var body = transform.Find("Body");
        size = new Vector3(0.925f, 0.4f, 0.1f);
        lifetime = 5f;
        location = body;
        offset = new Vector3(0f, 0.6f, 0f);
        fadeInDuration = fadeOutDuration = 0.2f;
        movementMode = Tippy.MovementMode.Exact;

        dSpin = new Vector3(0, 0, -12f);
        text = transform.Find("Text").GetComponent<TMPro.TextMeshPro>();
        logo = transform.Find("Logo").gameObject;
        spinner = transform.Find("Spinner").gameObject;

        stampPosition = new Vector3(logo.transform.localPosition.x, 0f, logo.transform.localPosition.z);
        matryxPosition = logo.transform.localPosition;

        var texture = Resources.Load<Texture2D>("Icons/timestamp");
        var rect = new Rect(0f, 0f, texture.width, texture.height);
        var pivot = logo.GetComponent<SpriteRenderer>().sprite.pivot;
        stampSprite = Sprite.Create(texture, rect, new Vector2(0.56f, 0.52f));
        matryxSprite = logo.GetComponent<SpriteRenderer>().sprite;
    }

    public override bool shouldShowTippy()
    {
        return state != CommitButtonState.Disabled;
    }

    public override string textToDisplay()
    {
        switch(state)
        {
            case CommitButtonState.YetToBeClaimed:
                return "Timestamp\nto\nMatryx";
            case CommitButtonState.Processing:
                return "Sending\nEthereum\nTransation";
            case CommitButtonState.YetToBeCommitted:
                return "Commit\nto\nMatryx";
            case CommitButtonState.Committed:
                return "Your work\nhas been\ncommitted";
            default:
                return "Quack.";
        }
    }

    // Update is called once per frame
    void FixedUpdate()
    {
        if(spin)
        {
            spinner.transform.Rotate(dSpin);
        }
    }

    public enum CommitButtonState
    {
        Disabled = -1,
        Processing,
        YetToBeClaimed,
        YetToBeCommitted,
        Committed,
        CantCommit
    }

    internal void Spin()
    {
        spinner.SetActive(true);
        spin = true;
    }

    internal void NoSpin()
    {
        spinner.SetActive(false);
        spin = false;
    }

    public CommitButtonState GetState()
    {
        return state;
    }

    public void SetState(CommitButtonState newState)
    {
        state = newState;
        switch(newState)
        {
            case CommitButtonState.Processing:
                Spin();
                text.gameObject.SetActive(false);
                logo.SetActive(false);
                break;
            case CommitButtonState.YetToBeClaimed:
                NoSpin();
                text.gameObject.SetActive(false);
                logo.SetActive(true);
                logo.GetComponent<SpriteRenderer>().sprite = stampSprite;
                logo.transform.localPosition = stampPosition;
                break;
            case CommitButtonState.YetToBeCommitted:
                NoSpin();
                text.gameObject.SetActive(true);
                text.GetComponent<TMPro.TextMeshPro>().text = "Commit\nto\n";
                logo.GetComponent<SpriteRenderer>().sprite = matryxSprite;
                logo.transform.localPosition = matryxPosition;
                logo.SetActive(true);
                break;
            case CommitButtonState.Committed:
                NoSpin();
                GetComponent<FlexButtonComponent>().SetState(-1);
                text.gameObject.SetActive(true);
                text.GetComponent<TMPro.TextMeshPro>().text = "\nCommitted!";
                logo.SetActive(false);
                break;
            case CommitButtonState.CantCommit:
                NoSpin();
                GetComponent<FlexButtonComponent>().SetState(-1);
                text.gameObject.SetActive(true);
                text.GetComponent<TMPro.TextMeshPro>().text = "Cannot\nClaim!";
                logo.SetActive(false);
                break;
        }

        if(newState == CommitButtonState.Disabled)
        {
            GetComponent<FlexActionableComponent>().SetState(-1);
        }
        else
        {
            GetComponent<FlexActionableComponent>().SetState(0);
        }
    }


}