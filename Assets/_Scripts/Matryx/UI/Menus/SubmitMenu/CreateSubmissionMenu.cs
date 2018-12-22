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
using Calcflow.UserStatistics;

public class CreateSubmissionMenu : MonoBehaviour {

    string submitEndpoint = "http://13.57.11.64/v1/submit/";
    MatryxTournament tournament;
    [SerializeField]
    CustomParametrizedSurface customParametrizedSurface;
    [SerializeField]
    InputField TournamentField;
    [SerializeField]
    InputField TitleField;
    [SerializeField]
    AddressListModifier ContributorsList;
    [SerializeField]
    AddressListModifier ReferencesList;
    [SerializeField]
    Text InvalidLabel;
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
        TournamentField.text = tournament.title;
    }

    public void MakeSubmission()
    {
        var title = TitleField.text;
        if(!TitleField.gameObject.GetComponent<InputValidator>().isValid)
        {
            InvalidLabel.gameObject.SetActive(true);
            return;
        }

        this.gameObject.SetActive(false);
        InvalidLabel.gameObject.SetActive(false);

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
                    StatisticsTracking.EndEvent("Matryx", "Submission Creation");
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
        TitleField.text = "";
        ContributorsList.RemoveAll();
        ReferencesList.RemoveAll();
    }

    public bool ValidateInputs()
    {
        if (!TitleField.GetComponent<InputValidator>().isValid)
        {
            InvalidLabel.gameObject.SetActive(true);
            InvalidLabel.text = "Invalid Title field";
            return false;
        }
        //if (!Description.GetComponent<InputValidator>().isValid)
        //{
        //    InvalidLabel.gameObject.SetActive(true);
        //    InvalidLabel.text = "Invalid Description field";
        //    return false;
        //}

        //if (Bounty.CurrentValue == 0)
        //{
        //    InvalidLabel.gameObject.SetActive(true);
        //    InvalidLabel.text = "Tournament must have non-zero bounty";
        //    return false;
        //}

        //if (Duration.CurrentValue == 0)
        //{
        //    InvalidLabel.gameObject.SetActive(true);
        //    InvalidLabel.text = "Tournament must last at least one hour";
        //    return false;
        //}

        return true;
    }
}
