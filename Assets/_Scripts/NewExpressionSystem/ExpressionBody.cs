using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ExpressionBody : QuickButton
{
    Expressions expression;
    Transform expressionParent;
    Transform panel;
    Transform feedBack;
    TMPro.TextMeshPro textInput;
    string title = "X";
    OutputManager outputManager;
    CalcInput calcInput;
    ParametricExpression param;
    ParametricManager calcManager;

    private bool thisBodySelected = false;
    private bool finishedScalingUp = true;
    private bool finishedScalingDown = true;
    private bool variable = false;

    private Vector3 idleScale, selectedScale;

    private IEnumerator scaleUp, scaleDown;

    private void Awake()
    {
        calcManager = ParametricManager._instance;
        expression = GameObject.Find("Expressions").GetComponent<Expressions>();
        feedBack = transform.parent.Find("Feedback");

        if (transform.parent.parent.Find("VariableTitle")) variable = true;

        if (!variable) title = transform.parent.Find("Title").GetComponent<TMPro.TextMeshPro>().text.Substring(0, 1);

        if (transform.parent.Find("Text_Input"))
            textInput = transform.parent.Find("Text_Input").GetComponent<TMPro.TextMeshPro>();

        outputManager = expression.GetComponent<OutputManager>();
        calcInput = CalcInput._instance;

        selectedScale = (variable) ? new Vector3(0.7f, 0.04f, 0.002f) :
                                     new Vector3(4.3f, 0.04f, 0.002f);
        idleScale = new Vector3(0f, 0.04f, 0.002f);
    }

    protected override void Start()
    {
        base.Start();
    }

    public void setExpressionParent(Transform p)
    {
        expressionParent = p;
    }

    public Transform getExpressionParent()
    {
        return expressionParent;
    }

    public void setPanel(Transform p)
    {
        panel = p;
    }

    public Transform getPanel()
    {
        return panel;
    }

    public Transform getFeedBack()
    {
        return feedBack;
    }

    public TMPro.TextMeshPro getTextInput()
    {
        return textInput;
    }

    public void setTitle(string t)
    {
        transform.parent.Find("Title").GetComponent<TMPro.TextMeshPro>().text = t;
        title = t;
    }

    public string getTitle()
    {
        return title;
    }

    public bool isVariable()
    {
        return variable;
    }

    //NOTE: sets selected expression and body to be null
    public void unSelect()
    {
        if (!finishedScalingUp)
        {
            StopCoroutine(scaleUp);
            finishedScalingUp = true;
        }

        scaleDown = ScaleTo(feedBack, feedBack.localScale, idleScale, 0.5f);
        StartCoroutine(scaleDown);
        finishedScalingDown = false;

        expression.setSelectedExpr(null, null);
        thisBodySelected = false;
    }

    public void deselectCurrBody()
    {
        ExpressionBody selectedBody = expression.getSelectedBody();
        if (selectedBody)
        {
            TMPro.TextMeshPro oldTextInput = selectedBody.getTextInput();
            oldTextInput.text = oldTextInput.text.Replace("_", "");
            param = selectedBody.getExpressionParent().GetComponent<ParametricExpression>();
            param.getExpActions().disableButtons();
            unSelect();
        }
    }

    public void deselectPrevBody()
    {
        ExpressionBody selectedBody = expression.getSelectedBody();
        if (selectedBody)
        {
            TMPro.TextMeshPro oldTextInput = selectedBody.getTextInput();
            oldTextInput.text = oldTextInput.text.Replace("_", "");
            param = selectedBody.getExpressionParent().GetComponent<ParametricExpression>();
            param.getExpActions().disableButtons();

            if (selectedBody.transform != transform)
            {
                selectedBody.unSelect();
            }
        }
    }

    public void selectBody()
    {
        deselectPrevBody();

        expression.setSelectedExpr(expressionParent, this);
        param = expressionParent.GetComponent<ParametricExpression>();
        calcManager.ChangeExpressionSet(param.getExpSet());

        if (variable)
        {
            title = transform.parent.parent.Find("VariableTitle").Find("Title").GetComponent<TMPro.TextMeshPro>().text;
            outputManager.HandleInput(transform.parent.name, title);
        }
        else
        {
            calcManager.SetOutput(calcManager.expressionSet.GetExpression(title));
        }

        if (!finishedScalingDown)
        {
            StopCoroutine(scaleDown);
            finishedScalingDown = true;
        }

        scaleUp = ScaleTo(feedBack, feedBack.localScale, selectedScale, 0.3f);
        StartCoroutine(scaleUp);
        finishedScalingUp = false;
        thisBodySelected = true;
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        if (thisBodySelected)
        {
            deselectCurrBody();
        }
        else
        {
            param = expressionParent.GetComponent<ParametricExpression>();
            if (param.getActiveStatus()) selectBody();
        }
    }

    protected override void ButtonExitBehavior(GameObject other) { }

    IEnumerator ScaleTo(Transform obj, Vector3 start, Vector3 end, float overTime)
    {
        float startTime = Time.time;

        if (end == selectedScale) obj.gameObject.SetActive(true);

        while (Time.time < startTime + overTime)
        {
            obj.localScale = Vector3.Lerp(start, end, (Time.time - startTime) / overTime);
            yield return null;
        }

        obj.localScale = end;
        if (end == idleScale)
        {
            obj.gameObject.SetActive(false);
            finishedScalingDown = true;
        } 
        else if (end == selectedScale)
        {
            finishedScalingUp = true;
        }
    }

    private void OnDisable()
    {
        if (feedBack && feedBack.localScale == selectedScale)
        {
            feedBack.localScale = idleScale;
            feedBack.gameObject.SetActive(false);

            if (thisBodySelected)
            {
                ExpressionBody selectedBody = expression.getSelectedBody();
                TMPro.TextMeshPro oldTextInput = selectedBody.getTextInput();
                oldTextInput.text = oldTextInput.text.Replace("_", "");
                expression.setSelectedExpr(null, null);
                thisBodySelected = false;
                calcInput.ChangeOutput(null, calcManager);
            }
        }
    }

    void Update() { }
}
