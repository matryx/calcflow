using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class ResultsMenu : MonoBehaviour {
    [SerializeField]
    Text resultsText;

	public void PostSuccess(Matryx_Tournament tournament)
    {
        resultsText.text = "Successfully Submitted to " + tournament.address;
    }

    public void PostFailure(Matryx_Tournament tournament)
    {
        resultsText.text = "Failure Submitting to " + tournament.address;
    }

    public void ReturnToCalcflow()
    {
        this.gameObject.SetActive(false);
    }
}
