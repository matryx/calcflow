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

    MatryxTournament tournament;
    [SerializeField]
    CustomParametrizedSurface customParametrizedSurface;
    [SerializeField]
    InputField TournamentField;
    [SerializeField]
    InputField TitleField;
    [SerializeField]
    InputField DescriptionField;
    [SerializeField]
    InputField ValueField;
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
        if (!TitleField.gameObject.GetComponent<InputValidator>().isValid)
        {
            InvalidLabel.gameObject.SetActive(true);
            return;
        }

        this.gameObject.SetActive(false);
        InvalidLabel.gameObject.SetActive(false);

        var submission = new MatryxSubmission(tournament, title, "", DescriptionField.text, SerializeSurface(), Convert.ToInt32(ValueField.text));
        clearInputs();

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
                    ResultsMenu.Instance.PostSuccess(submission,
                        (nothin) => 
                        {
                            TournamentMenu.Instance.ReloadSubmissions(3f);
                        }
                    );
                    
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
        DescriptionField.text = "";
        ValueField.text = "";
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
