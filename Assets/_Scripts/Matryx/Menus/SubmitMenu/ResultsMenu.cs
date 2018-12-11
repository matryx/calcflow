using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

using Matryx;

public class ResultsMenu : MonoBehaviour {
    [SerializeField]
    Text resultsText;

	public void PostSuccess(MatryxTournament tournament)
    {
        resultsText.text = "Successfully Submitted to \n" + tournament.title;
    }

    public void PostFailure(MatryxTournament tournament)
    {
        resultsText.text = "Failure Submitting to " + tournament.address;
    }

    public void ReturnToCalcflow()
    {
        this.gameObject.SetActive(false);
    }
}
