using UnityEngine;

public class ClaimCommitButton : MonoBehaviour
{
    public static ClaimCommitButton Instance { get; private set; }
    CommitButtonState state;
    bool spin = false;
    Vector3 dSpin;
    GameObject text;
    GameObject logo;
    GameObject spinner;

    Vector3 stampPosition;
    Vector3 matryxPosition;

    Sprite stampSprite;
    Sprite matryxSprite;

    // Use this for initialization
    void Start()
    {
        if (Instance == null) { Instance = this; }
        dSpin = new Vector3(0, 0, -12f);
        text = transform.Find("Text").gameObject;
        logo = transform.Find("Logo").gameObject;
        spinner = transform.Find("Spinner").gameObject;

        stampPosition = new Vector3(logo.transform.localPosition.x, -0.84f, logo.transform.localPosition.z);
        matryxPosition = new Vector3(logo.transform.localPosition.x, -0.988f, logo.transform.localPosition.z);

        var texture = Resources.Load<Texture2D>("Icons/timestamp");
        var rect = new Rect(0f, 0f, texture.width, texture.height);
        var pivot = logo.GetComponent<SpriteRenderer>().sprite.pivot;
        stampSprite = Sprite.Create(texture, rect, new Vector2(0.56f, 0.52f));
        matryxSprite = logo.GetComponent<SpriteRenderer>().sprite;
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
        Committed
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
                text.SetActive(false);
                logo.SetActive(false);
                break;
            case CommitButtonState.YetToBeClaimed:
                NoSpin();
                text.SetActive(false);
                text.GetComponent<TextMesh>().text = "";
                logo.SetActive(true);
                logo.GetComponent<SpriteRenderer>().sprite = stampSprite;
                logo.transform.localPosition = stampPosition;
                break;
            case CommitButtonState.YetToBeCommitted:
                NoSpin();
                text.SetActive(true);
                text.GetComponent<TextMesh>().text = "Commit\nto\n";
                logo.GetComponent<SpriteRenderer>().sprite = matryxSprite;
                logo.transform.localPosition = matryxPosition;
                logo.SetActive(true);
                break;
            case CommitButtonState.Committed:
                NoSpin();
                GetComponent<FlexButtonComponent>().SetState(-1);
                text.SetActive(true);
                text.GetComponent<TextMesh>().text = "\nCommitted!";
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