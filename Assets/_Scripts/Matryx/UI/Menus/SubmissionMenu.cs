using UnityEngine;

using System;
using System.Collections;
using System.Collections.Generic;

using Matryx;

public class SubmissionMenu : MonoBehaviour
{

    [SerializeField]
    private TMPro.TextMeshPro titleText;
    [SerializeField]
    private TMPro.TextMeshPro bodyText;
    [SerializeField]
    private Material imageMaterial;
    [SerializeField]
    private ImportSubmission importSubmissionButton;

    MatryxSubmission submission;
    MatryxSubmission Submission
    { get { return submission; }
      set { submission = value; UpdateSubmissionDisplay(); } 
    }

    public void SetSubmission(MatryxSubmission submission)
    {
        titleText.text = submission.details.title;
        importSubmissionButton.Disable();

        if (this.submission == null ||
            this.submission.address != submission.address)
        {
            this.submission = submission;
            MatryxExplorer.RunFetchSubmission(submission, delegate (object results) { Submission = (MatryxSubmission)results; });
        }
    }

    void UpdateSubmissionDisplay()
    {
        titleText.text = submission.details.title;
        bodyText.text = submission.description;

        // Update the import button!
        importSubmissionButton.submission = submission;
        importSubmissionButton.Reenable();
    }
}
