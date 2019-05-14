using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class FlexHoverTipper : MonoBehaviour {

    public float fontSize;
    public TMPro.TextAlignmentOptions alignment;
    public Vector3 size;
    public float lifetime;
    public Transform location;
    public Vector3 offset;
    public float fadeInDuration;
    public float fadeOutDuration;
    public Tippy.MovementMode movementMode;

    public string initialText;
    protected TMPro.TextMeshPro text;

    public virtual bool shouldShowTippy()
    {
        return true;
    }

    public virtual string textToDisplay()
    {
        if (text != null)
        {
            bool greaterThanOneFifteen = text.text.Length > 115;
            var ellipse = greaterThanOneFifteen ? "..." : "";
            var length = greaterThanOneFifteen ? 115 : text.text.Length;
            return text.text.Substring(0, length) + ellipse;
        }
        else if (initialText.Length > 0)
        {
            return initialText.Replace(@"\\", @"\");
        }

        return string.Empty;
    }

    private Tippy tippy;
    private IEnumerator waitCoroutine;

    // Use this for initialization
    protected void Start () {
        var flexButton = GetComponent<FlexButtonComponent>();
        var textObj = transform.Find("Text");
        if(textObj != null)
        {
            text = textObj.GetComponent<TMPro.TextMeshPro>();
            if (text != null)
            {
                text.overflowMode = TMPro.TextOverflowModes.Page;
            }
        }

        if (text != null || initialText.Length > 0)
        {
            GetComponentInChildren<RayCastReceiver>().OnRayCastStart += (sender) =>
            {
                waitCoroutine = waitAndCreateTippy();
                StartCoroutine(waitCoroutine);
            };

            GetComponentInChildren<RayCastReceiver>().OnRayCastEnd += (sender) =>
            {
                if (waitCoroutine != null)
                {
                    StopCoroutine(waitCoroutine);
                }
                fadeAndDieEarly();
                if (flexButton.State != -1)
                {
                    flexButton.SetState(0);
                }
            };
        }
    }

    public void createTippy(string content)
    {
        // var body = transform.Find("Body");
        tippy = Tippies.SpawnTippy(content, fontSize, alignment, size, lifetime, gameObject.transform, offset, fadeInDuration, fadeOutDuration, movementMode);
    }

    public IEnumerator waitAndCreateTippy()
    {
        string text = textToDisplay();

        if (tippy == null &&
            text.Length > 0 &&
            shouldShowTippy())
        {
            yield return new WaitForSeconds(0.5f);
            createTippy(textToDisplay());
        }
    }

    public void fadeAndDieEarly()
    {
        if (tippy != null)
        {
            tippy.fadeEarly(tippy.fadeOutDuration, (obj) => { Destroy(tippy.gameObject); });
        }
    }
}
