using System.Collections;
using System.Collections.Generic;
using UnityEngine;

using Matryx;

public class TournamentContainer : MonoBehaviour
{
    public MatryxTournament tournament { get; set; }

    Tippy tippy;
    IEnumerator waitCoroutine;

    public void Start()
    {
        var flexButton = GetComponent<FlexButtonComponent>();
        if (tournament != null)
        {
            flexButton.SetStayCallback((button, source) =>
            {
                waitCoroutine = waitAndCreateTippy(tournament.title);
                StartCoroutine(waitCoroutine);
            });

            flexButton.SetExitCallback((button, source) =>
            {
                StopCoroutine(waitCoroutine);
                fadeAndDieEarly();
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
            tournament != null &&
            content.Length > 0)
        {
            yield return new WaitForSeconds(0.5f);
            createTippy(content);
        }
    }

    public void fadeAndDieEarly()
    {
        if (tippy != null)
        {
            tippy.fadeEarly((obj) => { Destroy(tippy.gameObject); });
        }
    }
}
