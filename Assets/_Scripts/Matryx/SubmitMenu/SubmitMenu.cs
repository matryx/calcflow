using System.Collections;
using System.Linq;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using UnityEngine.Networking;
using TMPro;
using System;
using System.Net;
using System.Text;
using System.IO;

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

    [SerializeField]
    GameObject submittingCanvasObject;
    [SerializeField]
    GameObject resultsCanvasObject;

    public void SetTournament(Matryx_Tournament tournament)
    {
        this.tournament = tournament;
        Tournament_InputField.text = tournament.title;
    }

    public void MakeSubmission()
    {
        Debug.Log("Making submission...");

        var title = Title_InputField.text;
        if (title == "" || title == null)
        {
            Title_InputField.GetComponent<Image>().color = new Color(1f, 181f / 255f, 181f / 255f);
            return;
        }

        var contributorsList = ContributorsList.GetAddressList();
        var referencesList = ReferencesList.GetAddressList();
        var bodyData = SerializeSurface();
        
        Matryx_Submission submission = new Matryx_Submission(title, contributorsList, referencesList, bodyData);
        string responseFromServer = UploadSubmission(tournament, submission);
    }

    string UploadSubmission(Matryx_Tournament tournament, Matryx_Submission submission)
    {
        submittingCanvasObject.SetActive(true);


        WebRequest request = WebRequest.Create(submitEndpoint);
        request.Method = "POST";

        string referencesString = "[" + String.Join(",", submission.contributors.ToArray()) + "]";
        string contributorsString = "[" + String.Join(",", submission.references.ToArray()) + "]";
        string postData = "tournamentID="+tournament.address+"&title="+submission.title+"&references="+ referencesString + "&contributors=" + contributorsString + "&submissionBody="+submission.body;
        byte[] byteArray = Encoding.UTF8.GetBytes(postData);
        request.ContentType = "application/x-www-form-urlencoded";
        // Set the ContentLength property of the WebRequest.
        request.ContentLength = byteArray.Length;

        // Get the request stream.
        Stream dataStream = request.GetRequestStream();
        // Write the data to the request stream.
        dataStream.Write(byteArray, 0, byteArray.Length);
        // Close the Stream object.
        dataStream.Close();

        // Get the response.
        WebResponse response = request.GetResponse();

        // Switch out the submitting screen for the results screen.
        submittingCanvasObject.SetActive(false);
        resultsCanvasObject.SetActive(true);
        this.gameObject.SetActive(false);

        // Display the status.
        if (((HttpWebResponse)response).StatusDescription == "OK")
        {
            Debug.Log("Submission uploaded successfully!");
            resultsCanvasObject.GetComponent<ResultsMenu>().PostSuccess(tournament);
        }
        else
        {
            resultsCanvasObject.GetComponent<ResultsMenu>().PostFailure(tournament);
        }

        Debug.Log(((HttpWebResponse)response).StatusDescription);
        // Get the stream containing content returned by the server.
        dataStream = response.GetResponseStream();
        // Open the stream using a StreamReader for easy access.
        StreamReader reader = new StreamReader(dataStream);
        // Read the content.
        string responseFromServer = reader.ReadToEnd();
        JSONObject jsonResponse = new JSONObject(responseFromServer);
        
        // Clean up the streams.
        reader.Close();
        dataStream.Close();
        response.Close();
        
        return responseFromServer;
    }

    public string SerializeSurface()
    {
        List<SerializableExpressionSet> serializableExpressions = customParametrizedSurface.expressionSets.Select(x => new SerializableExpressionSet(x)).ToList();
        return JsonHelper.ToJson(serializableExpressions);
    }

}
