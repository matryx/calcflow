using UnityEngine;

using System;
using System.Collections;
using System.Collections.Generic;

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

    private string submissionEndpoint = "http://13.57.11.64/v1/submission/";

    Matryx_Submission submission;

    public void SetSubmission(Matryx_Submission submission)
    {
        titleText.text = submission.getTitle();
        importSubmissionButton.Disable();

        if (this.submission == null ||
            this.submission.address != submission.address)
        {
            this.submission = submission;
            MatryxJsonRpc.Request.RunDetailSubmission(submission.address, ProcessSubmission);
        }
    }

    void ProcessSubmission(object results)
    {
        var rpcSubmission = (MatryxJsonRpc.Submission)results;
        submission.title = rpcSubmission.title;
        submission.body = rpcSubmission.body;
        submission.contributors = rpcSubmission.contributorsList();
        submission.references = rpcSubmission.referencesList();
        // Update the display
        UpdateSubmissionDisplay();
    }

    /*
    void ProcessSubmissionOLD(string jsonString)
    {
        JSONObject jsonObject = new JSONObject(jsonString);
        jsonObject.GetField("results", delegate (JSONObject results)
        {
            results.GetField(submission.address, delegate (JSONObject jsonSubmission)
            {
                Debug.Log(jsonSubmission);
                string title = null;
                jsonSubmission.GetField("title", delegate (JSONObject titleObject)
                {
                    title = titleObject.str;
                });

                string body = null;
                jsonSubmission.GetField("body", delegate (JSONObject bodyObject)
                {
                    body = bodyObject.ToString();

                });

                List<string> contributors = new List<string>();
                List<string> references = new List<string>();

                jsonSubmission.GetField("contributors", delegate (JSONObject jsonContributors)
                {
                    foreach(JSONObject jsonContributor in jsonContributors.list)
                    {
                        contributors.Add(jsonContributor.str);
                    }
                });
                jsonSubmission.GetField("references", delegate (JSONObject jsonReferences)
                {
                    foreach (JSONObject jsonReference in jsonReferences.list)
                    {
                        references.Add(jsonReference.str);
                    }
                });


                submission.title = title;
                submission.body = body;
                submission.contributors = contributors;
                submission.references = references;

            });
        });
    }
    */

    void UpdateSubmissionDisplay()
    {
        titleText.text = submission.title;
        bodyText.text = submission.body;

        // Update the import button!
        importSubmissionButton.submission = submission;
        importSubmissionButton.Reenable();
    }
}
