using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Matryx_Submission
{
    public Matryx_Submission(string address)
    {
        this.address = address;
    }
    public Matryx_Submission(string title, string bodyAddress)
    {
        this.title = title;
        this.bodyAddress = bodyAddress;
    }

    public string address;
    private string title;
    private string bodyAddress;
    private string body;
    //public DateTime startTime;
    //public DateTime roundDuration;
    //public int round;
    //public DateTime reviewDuration;
    //public List<EthereumAddress> submissions;
    //public EthereumAddress updateAgentAddress;

    public string getTitle()
    {
        if (title != null)
        {
            return title;
        }

        //TODO: Implement
        return "tournament";

    }

    public string getBody()
    {
        if (body != null)
        {
            return body;
        }

        //TODO: Implement
        return "body of work from " + bodyAddress;
    }

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