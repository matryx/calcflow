using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public struct Matryx_Submission
{
    public Matryx_Submission(string name, string bodyAddress)
    {
        this.name = name;
        this.descriptionAddress = bodyAddress;
    }
    public string name;
    public string descriptionAddress;
    //public DateTime startTime;
    //public DateTime roundDuration;
    //public int round;
    //public DateTime reviewDuration;
    //public List<EthereumAddress> submissions;
    //public EthereumAddress updateAgentAddress;
}

public class SubmissionContainer : MonoBehaviour {

    Matryx_Submission submission;

    public void SetSubmission(Matryx_Submission submission)
    {
        this.submission = submission;
    }

    public Matryx_Submission GetSubmission()
    {
        return submission;
    }
}