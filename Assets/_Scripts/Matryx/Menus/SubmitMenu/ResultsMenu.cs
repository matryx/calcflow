using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

using Matryx;

public class ResultsMenu : MonoBehaviour {
    [SerializeField]
    public Text statusText;
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

    public void SetStatus(string text)
    {
        Instance.statusText.text = text;
    }

	public void PostSuccess(MatryxSubmission submission)
    {
        SetStatus("Successfully Submitted to \n" + submission.tournament.title);
        severalMinutesText.gameObject.SetActive(false);
        mayReturn.gameObject.SetActive(true);
        StartCoroutine(ReturnToCalcflowAfterSeconds(6f));
    }

    public void PostFailure(MatryxSubmission submission, string message=null)
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
        StartCoroutine(ReturnToCalcflowAfterSeconds(6f));
    }

    public void PostSuccess(MatryxTournament tournament)
    {
        SetStatus("Successfully Created \n" + tournament.title);
        severalMinutesText.gameObject.SetActive(false);
        mayReturn.gameObject.SetActive(true);
        StartCoroutine(ReturnToCalcflowAfterSeconds(6f));
    }

    public void PostFailure(MatryxTournament tournament, string message = null)
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
        StartCoroutine(ReturnToCalcflowAfterSeconds(6f));
    }

    public IEnumerator ReturnToCalcflowAfterSeconds(float seconds)
    {
        yield return new WaitForSeconds(seconds);

        Instance.gameObject.SetActive(false);
        if(CreateSubmissionMenu.Instance != null)
        {
            CreateSubmissionMenu.Instance.gameObject.SetActive(false);
        }
        if(CreateTournamentMenu.Instance != null)
        {
            CreateTournamentMenu.Instance.gameObject.SetActive(false);
        }

        CreateTournamentButton.Instance.ToggleOff();
        CreateSubmissionButton.Instance.ToggleOff();
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
