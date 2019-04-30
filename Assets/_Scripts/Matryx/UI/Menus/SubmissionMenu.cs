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
    private ImportCommit importCommitButton;

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
        if (this.submission == null ||
            this.submission.hash != submission.hash)
        {
            titleText.text = submission.title;
            bodyText.text = "Loading...";
            ClearPreview();
            DisableImport();
            this.submission = submission;
            MatryxCortex.GetSubmission(submission, (result) =>
            {
                Submission = (MatryxSubmission)result;
                if (Submission.calcflowCompatible)
                {
                    EnableImport("View");
                }
                else
                {
                    DisableImport("Incompatible");
                }
            });
        }
    }

    void UpdateSubmissionDisplay()
    {
        titleText.text = submission.title;
        bodyText.text = submission.description;
        LoadPreviewImage();

        // Update the import button!
        importCommitButton.commit = submission.commit;
        importCommitButton.Enable();
    }

    void LoadPreviewImage()
    {
        transform.Find("Preview").GetComponent<Renderer>().material.mainTexture = submission.commit.previewImage;
    }

    void ClearPreview()
    {
        transform.Find("Preview").GetComponent<Renderer>().material.mainTexture = null;
    }

    public void EnableImport(string text = "")
    {
        importCommitButton.Enable(text);
    }

    public void DisableImport(string text = "")
    {
        importCommitButton.Disable(text);
    }
}
