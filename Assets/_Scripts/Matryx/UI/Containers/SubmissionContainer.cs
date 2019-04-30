using System.Collections;
using System.Collections.Generic;
using UnityEngine;

using Matryx;

public class SubmissionContainer : MonoBehaviour
{
    public MatryxSubmission submission { get; set; }
    TMPro.TextMeshPro submissionTitleText;

    Tippy tippy;
    IEnumerator waitCoroutine;

    public void Start()
    {
        //submissionTitleText.textInfo.pageInfo.Length
        var flexButton = GetComponent<FlexButtonComponent>();
        if (submission != null)
        {
            flexButton.SetStayCallback((button, source) =>
            {
                waitCoroutine = waitAndCreateTippy(submission.title);
                StartCoroutine(waitCoroutine);
            });

            flexButton.SetExitCallback((button, source) =>
            {
                if (waitCoroutine != null)
                {
                    StopCoroutine(waitCoroutine);
                }
                fadeAndDieEarly();
                flexButton.SetState(0);
            });
        }
    }

    public void createTippy(string content)
    {
        var body = transform.Find("Body");
        tippy = Tippies.SpawnTippy(content, 1.1f, TMPro.TextAlignmentOptions.Left, new Vector2(1f, 0.8f), 120f, body, new Vector3(0f, -0.9f, -0.2f), 0.2f, 0.2f, Tippy.MovementMode.Exact);
    }

    public IEnumerator waitAndCreateTippy(string content)
    {
        if (tippy == null &&
            submission != null &&
            content.Length > 39)
        {
            yield return new WaitForSeconds(0.5f);
            createTippy(content);
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
