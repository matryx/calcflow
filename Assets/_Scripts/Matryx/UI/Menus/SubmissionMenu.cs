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

    public static SubmissionMenu Instance { get; private set; }

    public void OnEnable()
    {
        if(Instance == null)
        {
            Instance = this;
        }
    }

    public void SetSubmission(MatryxSubmission submission)
    {
        titleText.text = submission.title;
        importSubmissionButton.Disable();

        if (this.submission == null ||
            this.submission.hash != submission.hash)
        {
            DisableImport();
            this.submission = submission;
            MatryxCortex.GetSubmission(submission, (result) =>
            {
                Submission = (MatryxSubmission)result;
                if (Submission.calcflowCompatible)
                {
                    EnableImport();
                }
                else
                {
                    DisableImport();
                }
            });
        }
    }

    void UpdateSubmissionDisplay()
    {
        titleText.text = submission.title;
        bodyText.text = submission.description;

        // Update the import button!
        importSubmissionButton.submission = submission;
        importSubmissionButton.Reenable();
    }

    public void EnableImport()
    {
        importSubmissionButton.Reenable();
    }

    public void DisableImport()
    {
        importSubmissionButton.Disable();
    }
}
