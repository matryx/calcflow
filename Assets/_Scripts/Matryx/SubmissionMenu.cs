using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Web;

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
            WebLoader.Instance.Load(submissionEndpoint + "?id=" + submission.address, ProcessSubmission);
        }
    }

    void ProcessSubmission(string jsonString)
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

                // Update the display
                UpdateSubmissionDisplay();
            });
        });
    }

    void UpdateSubmissionDisplay()
    {
        titleText.text = submission.title;
        bodyText.text = submission.body;

        // Update the import button!
        importSubmissionButton.submission = submission;
        importSubmissionButton.Reenable();
    }
}
