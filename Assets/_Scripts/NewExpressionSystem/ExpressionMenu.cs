using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ExpressionMenu : MonoBehaviour {
    Scroll expressionScroll;
    List<Transform> expressions;
    public enum Action { Remove, Hide, Flowline }

    //NOTES: managing expressions
    // - need to keep track of selected Expr
    // - selected Expr affects which Actions are shown, the graph and output destination

    void Awake()
    {
        expressionScroll = GetComponentInChildren<Scroll>();
        expressionScroll.addObject(transform.Find("ExpressionSelector"));

        if (transform.Find("Panel").childCount == 0)
        {
            transform.Find("Actions").Find("Remove").gameObject.SetActive(false);
            transform.Find("Actions").Find("Hide").gameObject.SetActive(false);
            transform.Find("Actions").Find("Flowline").gameObject.SetActive(false);
        }
    }

    void Update()
    {

    }
}
