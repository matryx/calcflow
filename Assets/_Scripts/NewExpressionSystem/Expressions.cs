using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Expressions : MonoBehaviour
{

    //internal class Expression 
    //{
    //    ExpressionType etype;
    //    List<Transform> components;
    //    List<Action> actions;
    //}

    Scroll expressionScroll;
    //List<Expression> expressions;
    //Expression selectedExpr;
    public enum ExpressionType { Constant, Paramet, VecField }
    public enum Action { Add, Remove, Hide, Flowline }

    //NOTES: managing expressions
    // - need to keep track of selected Expr
    // - selected Expr affects which Actions are shown, the graph and output destination

    void Awake()
    {
        expressionScroll = GetComponentInChildren<Scroll>();

        if (transform.Find("Panel").childCount == 0)
        {
            transform.Find("ExpressionSelector").gameObject.SetActive(true);
            transform.Find("Actions").Find("Remove").gameObject.SetActive(false);
            transform.Find("Actions").Find("Hide").gameObject.SetActive(false);
            transform.Find("Actions").Find("Flowline").gameObject.SetActive(false);
        }
        else
        {
            transform.Find("ExpressionSelector").gameObject.SetActive(false);
        }
    }

    void Update()
    {

    }
}
