using UnityEngine;

using System.Collections;
using System.Collections.Generic;

using Matryx;

public class SubmissionContainer : MonoBehaviour {

    MatryxSubmission submission;

    public void SetSubmission(MatryxSubmission submission)
    {
        this.submission = submission;
    }

    public MatryxSubmission GetSubmission()
    {
        return submission;
    }
}