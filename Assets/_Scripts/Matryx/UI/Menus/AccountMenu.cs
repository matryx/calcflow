using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

using Matryx;
using System.Numerics;
using Vector3 = UnityEngine.Vector3;

public class AccountMenu : MonoBehaviour
{
    //internal class AccountMenuResponder : FlexMenu.FlexMenuResponder
    //{
    //    public FlexMenu menu;
    //    AccountMenu accountMenu;
    //    internal AccountMenuResponder(AccountMenu accountMenu, FlexMenu menu)
    //    {
    //        this.menu = menu;
    //        this.accountMenu = accountMenu;
    //    }

    //    public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
    //    {
    //        AccountMenu.HandleInput(sender.gameObject);
    //    }

    //    public void Flex_ActionEnd(string name, FlexActionableComponent sender, GameObject collider) { }
    //}

    //private static void HandleInput(GameObject source)
    //{
    //    if (source.name == "Sign Out")
    //    {
    //        //SignOut();
    //    }
    //    else if (source.name.Contains("Account: "))
    //    {
    //        // SwitchAccount();
    //    }
    //}

    //AccountMenuResponder responder;
    //SelectorFlexPanel accountPanel;
    //private Scroll scroll;
    //const int maxTextLength = 400;
    //JoyStickAggregator joyStickAggregator;
    //FlexMenu flexMenu;

    //public void Initialize(CalcManager calcManager)
    //{
    //    scroll = GetComponentInChildren<Scroll>(true);
    //    flexMenu = GetComponent<FlexMenu>();
    //    AccountMenuResponder responder = new AccountMenuResponder(this, flexMenu);
    //    flexMenu.RegisterResponder(responder);
    //    accountPanel = GetComponentInChildren<SelectorFlexPanel>().Initialize();
    //    joyStickAggregator = scroll.GetComponent<JoyStickAggregator>();
    //}

    ///// <summary>
    ///// Clears the list of tournaments.
    ///// </summary>
    //public void ClearAccounts()
    //{
    //    scroll.clear();
    //}

    //GameObject loadButton;
    //private void DisplaySubmissions(List<MatryxSubmission> submissions)
    //{
    //    if (submissions.Count == 0)
    //    {
    //        infoText.gameObject.SetActive(true);
    //        infoText.text = "No Submissions On This Tournament";
    //    }
    //    foreach (MatryxSubmission submission in submissions)
    //    {
    //        GameObject button = createButton(submission);
    //        button.SetActive(false);
    //        submissionsPanel.AddAction(button.GetComponent<FlexButtonComponent>());
    //    }
    //}

    //private GameObject createButton(MatryxSubmission submission)
    //{
    //    GameObject button = Instantiate(Resources.Load("Submission_Cell", typeof(GameObject))) as GameObject;
    //    button.transform.SetParent(accountPanel.transform);
    //    button.transform.localScale = Vector3.one;

    //    button.name = submission.title;
    //    button.GetComponent<SubmissionContainer>().submission = submission;

    //    var buttonText = button.transform.Find("Text").GetComponent<TMPro.TextMeshPro>();
    //    buttonText.text = submission.title;
    //    buttonText.alignment = TMPro.TextAlignmentOptions.Center;

    //    scroll.addObject(button.transform);
    //    joyStickAggregator.AddForwarder(button.GetComponentInChildren<JoyStickForwarder>());

    //    return button;
    //}

    
}
