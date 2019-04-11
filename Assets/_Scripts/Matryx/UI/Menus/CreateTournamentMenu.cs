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
    NumberPicker Duration;
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

        Duration.maxValue = 8760; // hours in one year
    }

    public void TakeInput()
    {
        if (!ValidateInputs()) return;

        InvalidText.gameObject.SetActive(false);

        BigInteger bounty = new BigInteger(Bounty.CurrentValue) * new BigInteger(1e18);
        BigInteger entryFee = new BigInteger(EntryFee.CurrentValue) * new BigInteger(1e18);

        System.DateTime epochStart = new System.DateTime(1970, 1, 1, 0, 0, 0, System.DateTimeKind.Utc);
        int currentTime = (int)(System.DateTime.UtcNow - epochStart).TotalSeconds;

        MatryxRound.RoundDetails roundDetails = new MatryxRound.RoundDetails()
        {
            Start = currentTime,
            Duration = new BigInteger(Duration.CurrentValue*60*60),
            Bounty = bounty,
            Review = 60 * 60 * 24 * 14
        };
        MatryxTournament tournament = new MatryxTournament(Title.text, Description.text, null, "math", bounty, entryFee, roundDetails);

        Async.runInCoroutine(delegate (Async thread, object param)
        {
            return tournament.create(delegate (object result)
            {
                // Check success
                if ((bool)result)
                {
                    StatisticsTracking.EndEvent("Matryx", "Tournament Creation");
                    ResultsMenu.Instance.PostSuccess(tournament);
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

        if(Bounty.CurrentValue == 0)
        {
            InvalidText.gameObject.SetActive(true);
            InvalidText.text = "Tournament must have non-zero bounty";
            return false;
        }

        if(Duration.CurrentValue == 0)
        {
            InvalidText.gameObject.SetActive(true);
            InvalidText.text = "Tournament must last at least one hour";
            return false;
        }

        return true;
    }

    public void updateLengthRequirement()
    {
        int currentLength = -10 + Description.text.Length;
        lengthRequirement.text = currentLength + "/1000";

        float red = 1f;
        float other = currentLength < 1000 ? 1 + (currentLength / 30f) : 0f;
        Color lengthColor = new Color(red, other, other);
        lengthRequirement.color = lengthColor;
    }
}
