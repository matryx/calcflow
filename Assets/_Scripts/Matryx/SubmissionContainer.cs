using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Matryx_Submission
{
    public Matryx_Submission(string title, List<string> contributors, List<string> references, string body)
    {
        this.title = title;
        this.contributors = contributors;
        this.references = references;
        this.body = body;
    }
    public Matryx_Submission(string address)
    {
        this.address = address;
    }
    public Matryx_Submission(string title, string address)
    {
        this.title = title;
        this.address = address;
    }

    public string address;
    public string title;
    public string bodyAddress;
    public string body;
    public List<string> contributors;
    public List<string> references;

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