using Matryx;
using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class CommitMenu : MonoBehaviour {

    public static CommitMenu Instance { get; private set; }
    MatryxCommit commit;
    [SerializeField]
    TMPro.TextMeshPro title;
    [SerializeField]
    TMPro.TextMeshPro creationDate;
    [SerializeField]
    TMPro.TextMeshPro description;
    [SerializeField]
    TMPro.TextMeshPro creator;
    [SerializeField]
    ParentCommitButton parentCommitButton;
    [SerializeField]
    ImportCommit importButton;
   
	void Start () {
		if(Instance == null)
        {
            Instance = this;
        }
	}

    public void SetCommit(MatryxCommit commit)
    {
        if (commit == null)
        {
            parentCommitButton.transform.parent.gameObject.SetActive(false);
        }
        else
        {
            parentCommitButton.transform.parent.gameObject.SetActive(true);
        }

        this.commit = commit;
        title.text = "Commit " + commit.hash.Substring(0, 10);
        var datetime = TimeZoneInfo.ConvertTime(Utils.Time.FromUnixTime((double)commit.timestamp), TimeZoneInfo.Local);
        creationDate.text = datetime.ToShortDateString() + ", " + datetime.ToLongTimeString();
        description.text = "Content: \n\n";
        creator.text = "by " + Utils.ellipseAddress(commit.owner);
        importButton.Disable();
        MatryxCortex.RunGetCommit(commit.hash, true, UpdateDisplay, ErrorGettingCommit);
    }

    public void SetCommit(string commitHash)
    {
        if(commitHash.Equals(""))
        {
            parentCommitButton.transform.parent.gameObject.SetActive(false);
        }
        else
        {
            parentCommitButton.transform.parent.gameObject.SetActive(true);
        }

        this.commit = new MatryxCommit(commitHash);
        title.text = "Commit " + commit.hash.Substring(0, 10);
        description.text = "Content: \n\n";
        importButton.Disable();
        MatryxCortex.RunGetCommit(commit.hash, true, UpdateDisplay, ErrorGettingCommit);
    }

    public void UpdateDisplay(object commit)
    {
        MatryxCommit niceCommit = (MatryxCommit)commit;
        description.text = "Content: \n\n" + niceCommit.content;
        importButton.commit = niceCommit;
        importButton.Enable();
        if(niceCommit.parentHash != null && niceCommit.parentHash != "")
        {
            parentCommitButton.parentCommitLabel.text = niceCommit.parentHash.Substring(0, 10);
        }
    }

    public void ErrorGettingCommit(object nothing)
    {
        description.text = "Commit content formatting error.";
    }
}
