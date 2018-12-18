using System.Collections;
using System.Linq;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using UnityEngine.Networking;
using TMPro;
using System;
using System.Net;
using System.Text;
using System.IO;

using Matryx;
using Nanome.Core;

public class CreateSubmissionMenu : MonoBehaviour {

    string submitEndpoint = "http://13.57.11.64/v1/submit/";
    MatryxTournament tournament;
    [SerializeField]
    CustomParametrizedSurface customParametrizedSurface;
    [SerializeField]
    InputField Tournament_InputField;
    [SerializeField]
    InputField Title_InputField;
    [SerializeField]
    AddressListModifier ContributorsList;
    [SerializeField]
    AddressListModifier ReferencesList;

    [SerializeField]
    GameObject submittingCanvasObject;
    [SerializeField]
    public Text submissionProgressText;

    [SerializeField]
    GameObject resultsCanvasObject;

    public static CreateSubmissionMenu Instance { get; private set; }

    public void Start()
    {
        if (Instance == null)
        {
            Instance = this;
        }
    }

    public void SetTournament(MatryxTournament tournament)
    {
        this.tournament = tournament;
        Tournament_InputField.text = tournament.title;
    }

    public void MakeSubmission()
    {
        this.gameObject.SetActive(false);

        var title = Title_InputField.text;
        if (title == "" || title == null)
        {
            Title_InputField.GetComponent<Image>().color = new Color(1f, 181f / 255f, 181f / 255f);
            return;
        }

        var contributors = ContributorsList.GetAddressList();
        var references = ReferencesList.GetAddressList();
        var bodyData = SerializeSurface();

        clearInputs();

        var submission = new MatryxSubmission(tournament, title, contributors, references, bodyData, "This submission was created with Calcflow.");
        Debug.Log("Submission: " + tournament.address + " -> " + title);

        resultsCanvasObject.SetActive(true);
        Async.runInCoroutine(delegate (Async thread, object param)
        {
            return submission.submit(delegate (object result)
            {
                this.gameObject.SetActive(false);
                // Debug
                Debug.Log("Submission uploaded");
                Debug.Log(result);
                // Check success
                if ((bool)result)
                {
                    ResultsMenu.Instance.PostSuccess(submission);
                }
                else
                {
                    ResultsMenu.Instance.PostFailure(submission);
                }
            });
        });
    }

    public string SerializeSurface()
    {
        List<SerializableExpressionSet> serializableExpressions = customParametrizedSurface.expressionSets.Select(x => new SerializableExpressionSet(x)).ToList();
        return JsonHelper.ToJson(serializableExpressions);
    }

    public void clearInputs()
    {
        Title_InputField.text = "";
        ContributorsList.RemoveAll();
        ReferencesList.RemoveAll();
    }
}
