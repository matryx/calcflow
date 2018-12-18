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
        float bounty = Bounty.CurrentValue;
        float entryFee = EntryFee.CurrentValue;

        System.DateTime epochStart = new System.DateTime(1970, 1, 1, 0, 0, 0, System.DateTimeKind.Utc);
        int currentTime = (int)(System.DateTime.UtcNow - epochStart).TotalSeconds;

        MatryxRound.RoundDetails roundDetails = new MatryxRound.RoundDetails()
        {
            Start = currentTime,
            End = new BigInteger(currentTime + Duration.CurrentValue*60*60),
            Bounty = new BigInteger(bounty),
            Review = 60 * 60 * 24 * 14
        };
        MatryxTournament tournament = new MatryxTournament(Title.text, Description.text, null, "math", new BigInteger(bounty) * new BigInteger(1e18), new BigInteger(entryFee) * new BigInteger(1e18), roundDetails);

        Async.runInCoroutine(delegate (Async thread, object param)
        {
            return tournament.create(delegate (object result)
            {
                // Check success
                if ((bool)result)
                {
                    ResultsMenu.Instance.PostSuccess(tournament);
                    StartCoroutine(ResultsMenu.Instance.ReturnToCalcflowAfterSeconds(10f));
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

    public static bool ValidateInput(string input)
    {
        return true;
    }
}
