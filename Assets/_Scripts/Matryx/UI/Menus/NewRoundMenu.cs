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
using System.Numerics;

public class NewRoundMenu : MonoBehaviour
{

    MatryxTournament tournament;

    [SerializeField]
    GameObject InvalidLabel;
    [SerializeField]
    GameObject currentStartText;
    [SerializeField]
    GameObject currentEndText;
    [SerializeField]
    GameObject currentReviewText;
    [SerializeField]
    DatePickerControl startDatePicker;
    [SerializeField]
    DatePickerControl endDatePicker;
    [SerializeField]
    DatePickerControl reviewEndDatePicker;
    [SerializeField]
    NumberPicker bountyPicker;
    [SerializeField]
    GameObject resultsCanvasObject;
    ResultsMenu resultsMenu;

    public static NewRoundMenu Instance { get; private set; }

    public void Start()
    {
        if (Instance == null)
        {
            Instance = this;
            resultsMenu = resultsCanvasObject.GetComponent<ResultsMenu>();
        }
    }

    public void OnEnable()
    {
        var roundDetailsCoroutine = new Utils.CoroutineWithData<MatryxRound.RoundDetails>(MatryxCortex.Instance, tournament.getRoundDetails(tournament.currentRound.index + 1,
            delegate (object res)
            {
                MatryxRound.RoundDetails details = (MatryxRound.RoundDetails)res;
                Debug.Log("Current start: " + details.Start);
                // create dates
                DateTime startDateTime = Utils.Time.FromUnixTime(details.Start);
                DateTime endDateTime = Utils.Time.FromUnixTime(details.Start + details.Duration);
                DateTime reviewDateTime = Utils.Time.FromUnixTime(details.Start + details.Duration + details.Review);

                currentStartText.SetActive(true);
                currentEndText.SetActive(true);
                currentReviewText.SetActive(true);
                currentStartText.GetComponent<Text>().text = "Current start date: \n" + startDateTime.ToShortDateString() + "\n" + startDateTime.ToLongTimeString();
                currentEndText.GetComponent<Text>().text = "Current end date: \n" + endDateTime.ToShortDateString() + "\n" + endDateTime.ToLongTimeString();
                currentReviewText.GetComponent<Text>().text = "Current review end date: \n" + reviewDateTime.ToShortDateString() + "\n" + reviewDateTime.ToLongTimeString(); ;
            },
            delegate (object err)
            {
                currentStartText.SetActive(false);
                currentEndText.SetActive(false);
                currentReviewText.SetActive(false);
            }
        ));
    }

    public void SetTournament(MatryxTournament tournament)
    {
        this.tournament = tournament;
    }

    public void CreateNewRound()
    {
        Async.runInCoroutine(delegate (Async thread, object param)
        {
            return runCreateRound();
        });
    }

    public IEnumerator runCreateRound()
    {
        InvalidLabel.gameObject.SetActive(false);

        if (!InputsValid().Equals(string.Empty))
        {
            yield break;
        }

        gameObject.SetActive(false);
        InvalidLabel.gameObject.SetActive(false);

        resultsCanvasObject.SetActive(true);

        var start = new BigInteger(Utils.Time.ToUnixTime(startDatePicker.fecha));
        var duration = new BigInteger((endDatePicker.fecha - startDatePicker.fecha).TotalSeconds);
        var reviewDuration = new BigInteger((reviewEndDatePicker.fecha - endDatePicker.fecha).TotalSeconds);
        var bounty = new BigInteger(Convert.ToDecimal(bountyPicker.CurrentValue));
        MatryxRound.RoundDetails details = new MatryxRound.RoundDetails()
        {
            Start = start,
            Duration = duration,
            Review = reviewDuration,
            Bounty = bounty * new BigInteger(1e18)
        };
        MatryxRound newRound = new MatryxRound(tournament.currentRound.index + 1, details);
        newRound.tournament = TournamentMenu.Tournament;

        ManageTournamentMenu.SetButtonsEnabled(false);

        resultsMenu.SetStatus("Checking MTX balance and platform allowance...");
        var allowance = new Utils.CoroutineWithData<BigInteger>(MatryxCortex.Instance, MatryxToken.allowance(NetworkSettings.currentAddress, MatryxPlatform.address));
        yield return allowance;

        var balance = new Utils.CoroutineWithData<BigInteger>(MatryxCortex.Instance, MatryxToken.balanceOf(NetworkSettings.currentAddress));
        yield return balance;

        if (balance.result < details.Bounty)
        {
            ResultsMenu.Instance.SetStatus("Insufficient MTX. Please visit <link=https://app.matryx.ai/><u>our Matryx Dapp</u></link> for MTX Tokens.", true);
            ResultsMenu.Instance.ReturnToCalcflowAfterSeconds(8f);
            yield break;
        }

        if (allowance.result < details.Bounty)
        {
            ResultsMenu.Instance.SetStatus("Approving MatryxPlatform for " + bounty + " MTX...");

            if (allowance.result != BigInteger.Zero)
            {
                var approveZero = new Utils.CoroutineWithData<bool>(MatryxCortex.Instance, MatryxToken.approve(MatryxPlatform.address, BigInteger.Zero));
                yield return approveZero;

                if (!approveZero.result)
                {
                    resultsMenu.PostFailure(newRound, "Failed to reset the platform allowance to zero");
                    yield break;
                }
            }

            var approveBounty = new Utils.CoroutineWithData<bool>(MatryxCortex.Instance, MatryxToken.approve(MatryxPlatform.address, details.Bounty));
            yield return approveBounty;

            if (!approveBounty.result)
            {
                resultsMenu.PostFailure(newRound, "Failed to give the platform an MTX allowance");
                yield break;
            }
        }

        resultsMenu.SetStatus("Adding to Tournament bounty...");
        var addToBountyCoroutine = new Utils.CoroutineWithData<bool>(MatryxCortex.Instance, TournamentMenu.Tournament.addToBounty(details.Bounty));
        yield return addToBountyCoroutine;

        if (!addToBountyCoroutine.result)
        {
            resultsMenu.PostFailure(newRound, "Could not add to tournament bounty");
            yield break;
        }

        resultsMenu.SetStatus("Creating new round...");
        var submissions = ManageTournamentMenu.winningSubmissions.Select(sub => Utils.HexStringToByteArray(sub.hash)).ToList();
        
        if (TournamentMenu.Tournament.currentRound.winningSubmissions.Count == 0)
        {
            IEnumerator selectWinnersCoroutine = tournament.selectWinners(submissions, ManageTournamentMenu.distribution, new BigInteger((int)MatryxTournament.SelectWinnerAction.StartNextRound), details.Start, details.Duration, details.Review, details.Bounty);
            var selectWinnersDataCoroutine = new Utils.CoroutineWithData<bool>(MatryxCortex.Instance, selectWinnersCoroutine);
            yield return selectWinnersDataCoroutine;

            ClearInputs();
            gameObject.SetActive(false);
            ManageTournamentMenu.SetButtonsEnabled(true);
            ManageTournamentMenu.Instance.PressButton("NewRoundButton");
            ResultsMenu.transactionObject = newRound;
            Debug.Log((bool)selectWinnersDataCoroutine.result ? "Successfully created new round!" : "Round creation unsuccessful.");

            if (selectWinnersDataCoroutine.result)
            {
                StatisticsTracking.EndEvent("Matryx", "New Round Creation", new Dictionary<string, object>() { { "success", true } });
                tournament.currentRound = newRound;
                resultsMenu.PostSuccess(newRound, 
                delegate (object nothin) 
                { 
                    TournamentMenu.Instance.SetRound(newRound.index);
                    TournamentMenu.Instance.UpdateActionState();
                    ManageTournamentMenu.Close();

                    TournamentMenu.Instance.actionState = TournamentMenu.ActionState.NoAction;
                    TournamentMenuCenterButton.Instance.updateState();
                });
            }
            else
            {
                StatisticsTracking.EndEvent("Matryx", "New Round Creation", new Dictionary<string, object>() { { "success", false } });
                resultsMenu.PostFailure(newRound, "Failed to create new round");
            }
        }
        else
        {
            resultsMenu.SetStatus("Updating next round...");
            IEnumerator updateNextRoundCoroutine = tournament.updateNextRound(details);
            var updateNextRoundDataCoroutine = new Utils.CoroutineWithData<bool>(MatryxCortex.Instance, updateNextRoundCoroutine);
            yield return updateNextRoundDataCoroutine;

            ClearInputs();
            gameObject.SetActive(false);
            ManageTournamentMenu.SetButtonsEnabled(true);
            ManageTournamentMenu.Instance.PressButton("NewRoundButton");

            if (updateNextRoundDataCoroutine.result)
            {
                StatisticsTracking.EndEvent("Matryx", "New Round Creation", new Dictionary<string, object>() { { "success", true } });
                tournament.currentRound = newRound;

                resultsMenu.PostSuccess(newRound, (nothin) => 
                {
                    TournamentMenu.Instance.actionState = TournamentMenu.ActionState.ManageTournament;
                    TournamentMenuCenterButton.Instance.updateState();
                }); 
            }
            else
            {
                StatisticsTracking.EndEvent("Matryx", "New Round Creation", new Dictionary<string, object>() { { "success", false } });
                resultsMenu.PostFailure(newRound, "Failed to create new round");
            }
        }
    }

    public void ClearInputs()
    {
        bountyPicker.setValue("");
    }

    public string InputsValid()
    {
        string error = "";
        if (startDatePicker.fecha < DateTime.Now - TimeSpan.FromMinutes(10))
        {
            error = "Round cannot start in the past";
        }
        else if(startDatePicker.fecha < tournament.currentRound.reviewEndDate)
        {
            error = "Round cannot begin before review period ends";
        }
        else if (endDatePicker.fecha < startDatePicker.fecha + TimeSpan.FromHours(1))
        {
            error = "Round must last at least one hour";
        }
        else if ((endDatePicker.fecha - startDatePicker.fecha).TotalDays > 365)
        {

            error = "Tournament duration must be 1 year or less";
        }
        else if (reviewEndDatePicker.fecha < endDatePicker.fecha)
        {
            error = "Tournament review must occur after tournament ends";
        }
        else if ((reviewEndDatePicker.fecha - endDatePicker.fecha).TotalDays < 1)
        {
            error = "Tournament review period must be at least one day";
        }
        else if ((reviewEndDatePicker.fecha - endDatePicker.fecha).TotalDays > 30)
        {
            error = "Tournament review period cannot exceed 30 days";
        }
        else if (bountyPicker.CurrentValue <= 0)
        {
            error = "Bounty must be greater than zero";
        }

        if (error.Length > 0)
        {
            InvalidLabel.gameObject.SetActive(true);
            InvalidLabel.GetComponent<Text>().text = error;
        }

        return error;
    }
}
