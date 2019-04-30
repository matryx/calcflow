using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

using Matryx;
using Nanome.Core;

public class ResultsMenu : MonoBehaviour {
    [SerializeField]
    public TMPro.TextMeshProUGUI statusText;
    [SerializeField]
    public Button dumbLinkButton;
    [SerializeField]
    public Text severalMinutesText;
    [SerializeField]
    public Text mayReturn;
    [SerializeField]
    public Button tryAgainButton;
    [SerializeField]
    public GameObject createTournamentCanvas;
    [SerializeField]
    public GameObject createSubmissionCanvas;

    public static ResultsMenu Instance { get; private set; }
    public static object transactionObject;

    public void Start()
    {
        if(Instance == null)
        {
            Instance = this;
        }
    }

    public void SetStatus(string text, bool useButton = false)
    {
        Instance.statusText.text = text;
        dumbLinkButton.gameObject.SetActive(useButton);
    }

	public void PostSuccess(MatryxSubmission submission, Async.EventDelegate onReturn = null)
    {
        SetStatus("Successfully Submitted to \n" + submission.tournament.title);
        severalMinutesText.gameObject.SetActive(false);
        mayReturn.gameObject.SetActive(true);
        StartCoroutine(ReturnToCalcflowAfterSeconds(3f, onReturn));
    }

    public void PostFailure(MatryxSubmission submission, string message=null, Async.EventDelegate onReturn = null)
    {
        if(message != null)
        {
            SetStatus(message);
        }
        else
        {
            SetStatus("Failure Submitting to " + submission.tournament.address);
        }

        severalMinutesText.gameObject.SetActive(false);
        mayReturn.gameObject.SetActive(true);
        tryAgainButton.gameObject.SetActive(true);
        StartCoroutine(ReturnToCalcflowAfterSeconds(3f, onReturn));
    }

    public void PostSuccess(MatryxTournament tournament, Async.EventDelegate onReturn = null)
    {
        SetStatus("Successfully Created \n" + tournament.title);
        severalMinutesText.gameObject.SetActive(false);
        mayReturn.gameObject.SetActive(true);
        StartCoroutine(ReturnToCalcflowAfterSeconds(3f, onReturn));
    }

    public void PostFailure(MatryxTournament tournament, string message = null, Async.EventDelegate onReturn = null)
    {
        if (message != null)
        {
            SetStatus(message);
        }
        else
        {
            SetStatus("Failure Creating " + tournament.title);
        }

        severalMinutesText.gameObject.SetActive(false);
        mayReturn.gameObject.SetActive(true);
        tryAgainButton.gameObject.SetActive(true);
        StartCoroutine(ReturnToCalcflowAfterSeconds(3f, onReturn));
    }

    public IEnumerator ReturnToCalcflowAfterSeconds(float seconds, Async.EventDelegate onReturn = null)
    {
        yield return new WaitForSeconds(seconds);

        mayReturn.gameObject.SetActive(false);
        severalMinutesText.gameObject.SetActive(true);
        tryAgainButton.gameObject.SetActive(false);
        Instance.gameObject.SetActive(false);
        if(CreateSubmissionMenu.Instance != null)
        {
            CreateSubmissionMenu.Instance.gameObject.SetActive(false);
        }
        if(CreateTournamentMenu.Instance != null)
        {
            CreateTournamentMenu.Instance.gameObject.SetActive(false);
        }

        if (CreateTournamentButton.Instance != null)
        {
            CreateTournamentButton.Instance.ToggleOff();
        }
        if (CreateSubmissionButton.Instance != null)
        {
            CreateSubmissionButton.Instance.ToggleOff();
        }

        onReturn?.Invoke(null);
    }

    public void TryAgain()
    {
        if(transactionObject is MatryxTournament)
        {
            createTournamentCanvas.SetActive(true);
            this.gameObject.SetActive(false);
        }
        else if(transactionObject is MatryxSubmission)
        {
            createSubmissionCanvas.SetActive(true);
            this.gameObject.SetActive(false);
        }
    }
}
