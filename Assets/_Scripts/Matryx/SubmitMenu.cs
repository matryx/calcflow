using System.Collections;
using System.Linq;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using UnityEngine.Networking;
using TMPro;
using System;

public class SubmitMenu : MonoBehaviour {

    string submitEndpoint = "http://13.57.11.64/v1/submit/";
    Matryx_Tournament tournament;
    [SerializeField]
    CustomParametrizedSurface customParametrizedSurface;
    [SerializeField]
    InputField Tournament_InputField;
    [SerializeField]
    InputField Title_InputField;
    [SerializeField]
    AddressListModifier ContributorsList;
    [SerializeField]
    AddressListModifier ReferencesList;

    public void SetTournament(Matryx_Tournament tournament)
    {
        this.tournament = tournament;
        Tournament_InputField.text = tournament.address;
    }

    public void MakeSubmission()
    {
        Debug.Log("Making submission...");

        var title = Title_InputField.text;
        var contributorsList = ContributorsList.GetAddressList();
        var referencesList = ReferencesList.GetAddressList();
        var bodyData = SerializeSurface();
        
        Matryx_Submission submission = new Matryx_Submission(title, contributorsList, referencesList, bodyData);
        StartCoroutine(UploadSubmission(submission));
    }

    IEnumerator UploadSubmission(Matryx_Submission submission)
    {
        List<IMultipartFormSection> formData = new List<IMultipartFormSection>();
        formData.Add(new MultipartFormDataSection("field1=foo&field2=bar"));
        formData.Add(new MultipartFormFileSection("my file data", "myfile.txt"));

        Dictionary<string, string> submissionFields = new Dictionary<string, string>();
        submissionFields.Add("title", submission.title);
        submissionFields.Add("references", "[" + String.Join(",", submission.references.ToArray()) + "]");
        submissionFields.Add("contributors", "[" + String.Join(",", submission.contributors.ToArray()) + "]");
        submissionFields.Add("submissionBody", submission.body);

        UnityWebRequest www = UnityWebRequest.Post(submitEndpoint, submissionFields);
        yield return www.Send();

        if (www.isError)
        {
            Debug.Log(www.error);
        }
        else
        {
            Debug.Log("Submission uploaded successfully!");
            Dictionary<string, string> responseHeaders = www.GetResponseHeaders();
            Debug.Log("Response: " + String.Join(",", responseHeaders.Values.ToArray<string>()));
        }
    }

    public string SerializeSurface()
    {
        List<SerializableExpressionSet> serializableExpressions = customParametrizedSurface.expressionSets.Select(x => new SerializableExpressionSet(x)).ToList();
        return JsonHelper.ToJson(serializableExpressions);
    }

}
