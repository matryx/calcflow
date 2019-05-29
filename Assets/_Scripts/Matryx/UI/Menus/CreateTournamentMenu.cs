using System;
using System.Collections;
using System.Text.RegularExpressions;
using System.Numerics;

using UnityEngine;
using UnityEngine.EventSystems;
using UnityEngine.UI;
using Valve.VR;

using Nanome.Core;
using Matryx;
using Calcflow.UserStatistics;

public class CreateTournamentMenu : MonoBehaviour
{

    [SerializeField]
    InputField Title;
    [SerializeField]
    NumberPicker Bounty;
    [SerializeField]
    NumberPicker EntryFee;
    [SerializeField]
    InputField Description;
    [SerializeField]
    DatePickerControl startDatePicker;
    [SerializeField]
    DatePickerControl endDatePicker;
    [SerializeField]
    Text lengthRequirement;

    [SerializeField]
    public Text InvalidText;

    [SerializeField]
    GameObject resultsMenu;

    public static CreateTournamentMenu Instance { get; private set; }

    public void Start()
    {
        if (Instance == null)
        {
            Instance = this;
        }
    }

    public void TakeInput()
    {
        if (!ValidateInputs()) return;

        InvalidText.gameObject.SetActive(false);

        BigInteger bounty = new BigInteger(Bounty.CurrentValue) * new BigInteger(1e18);
        BigInteger entryFee = new BigInteger(EntryFee.CurrentValue) * new BigInteger(1e18);

        double start = Utils.Time.ToUnixTime(startDatePicker.fecha);
        double end = Utils.Time.ToUnixTime(endDatePicker.fecha);
        double duration = (endDatePicker.fecha - startDatePicker.fecha).TotalSeconds;

        MatryxRound.RoundDetails roundDetails = new MatryxRound.RoundDetails()
        {
            Start = new BigInteger(start),
            Duration = new BigInteger(duration),
            Bounty = bounty,
            Review = 60 * 60 * 24 * 14
        };
        MatryxTournament tournament = new MatryxTournament(Title.text, Description.text, null, "math", bounty, entryFee, roundDetails);
        resultsMenu.GetComponent<ResultsMenu>().Start();

        Async.runInCoroutine(delegate (Async thread, object param)
        {
            return tournament.create(delegate (object result)
            {
                // Check success
                if ((bool)result)
                {
                    StatisticsTracking.EndEvent("Matryx", "Tournament Creation");
                    ResultsMenu.Instance.PostSuccess(tournament, 
                        (nothin) =>
                        {
                            Instance.ClearInputs(true);
                            TournamentsMenu.Instance.ReloadTournaments(0);
                        }
                    );
                }
                else
                {
                    ResultsMenu.Instance.PostFailure(tournament);
                }
            });
        });

        this.gameObject.SetActive(false);
        resultsMenu.SetActive(true);
    }

    public void Cancel()
    {
        Instance.gameObject.SetActive(false);
        CreateTournamentButton.Instance.ToggleOff();
    }

    public void ClearInputs(bool includeText = false)
    {
        Title.text = "";
        Bounty.CurrentValue = 0;
        EntryFee.CurrentValue = 0;
        Description.text = "";
    }

    public bool ValidateInputs()
    {
        if (!Title.GetComponent<InputValidator>().isValid)
        {
            InvalidText.gameObject.SetActive(true);
            InvalidText.text = "Invalid Title field";
            return false;
        }
        if (!Description.GetComponent<InputValidator>().isValid)
        {
            InvalidText.gameObject.SetActive(true);
            InvalidText.text = "Invalid Description field";
            return false;
        }

        if(Bounty.CurrentValue <= 0)
        {
            InvalidText.gameObject.SetActive(true);
            InvalidText.text = "Tournament must have non-zero bounty";
            return false;
        }

        DateTime now = DateTime.Now;
        if(startDatePicker.fecha < now && startDatePicker.fecha.Day < now.Day)
        {
            InvalidText.gameObject.SetActive(true);
            InvalidText.text = "Tournament cannot occur in the past";
            return false;
        }

        if(endDatePicker.fecha < startDatePicker.fecha)
        {
            InvalidText.gameObject.SetActive(true);
            InvalidText.text = "This isn't Back to the Future.";
            return false;
        }

        if((endDatePicker.fecha - startDatePicker.fecha).TotalDays > 365)
        {
            InvalidText.gameObject.SetActive(true);
            InvalidText.text = "Tournament duration must be 1 year or less.";
            return false;
        }

        return true;
    }

    public void updateLengthRequirement()
    {
        int currentLength = -10 + Description.text.Length;
        lengthRequirement.text = currentLength < 0 ? (-currentLength).ToString() : Description.text.Length + "/1000";

        float red = 1f;
        float other = currentLength < 1000 ? 1 + (currentLength / 30f) : 0f;
        Color lengthColor = new Color(red, other, other);
        lengthRequirement.color = lengthColor;
    }
}
