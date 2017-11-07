using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Web;

public class SubmissionMenu : MonoBehaviour {

    [SerializeField]
    private TMPro.TextMeshPro titleText;
    [SerializeField]
    private TMPro.TextMeshPro bodyText;

    public void SetSubmission(Matryx_Submission submission)
    {
        titleText.text = submission.getTitle();
    }
}
