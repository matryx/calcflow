using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class LinearTransManager : CalculatorManager 
{
    [HideInInspector]
    public MatrixSet matrixSet;

    //[HideInInspector]
    //public bool inputReceived;

    public static LinearTransManager _instance;
    JoyStickAggregator joyStickAggregator;
    Scroll linearTransScroll;
    List<MatrixSet> mtxSetList = new List<MatrixSet>();
    OutputManager outputManager;

    Expressions expressions;
    Transform selectedElement;
    //Variables in calcManager:

    //public bool updateOverlay = false;
    //internal bool toExport = false;

    //called by calculatorManager on start

	// Use this for initialization
	protected override void Initialize()
    {
        _instance = this;
        expressions = Expressions._instance;

        calcInput = CalcInput._instance;
        outputManager = OutputManager._instance;

        linearTransScroll = GameObject.Find("PanelBodyLinearTrans").transform.GetComponent<Scroll>();
        joyStickAggregator = linearTransScroll.GetComponent<JoyStickAggregator>();

        calcInput.Initialize(this);

        calcInput.ChangeOutput(matrixSet.getElement("A", 0, 0), this); //need to fix
        if (outputManager != null)
        {
            print("OUTPUT INIIALIZED");
            outputManager.Initialize(this);
        }

        ColorUtility.TryParseHtmlString("#64C3A7FF", out positiveFeedback);
    }

    private void addForwarders(Transform obj)
    {
        JoyStickForwarder[] forwarders = obj.GetComponentsInChildren<JoyStickForwarder>();
        foreach (JoyStickForwarder j in forwarders)
        {
            joyStickAggregator.AddForwarder(j);
        }
    }

    public void AddMatrixSet(MatrixSet MS)
    {
        mtxSetList.Add(MS);
        inputReceived = true;
    }

    public void RemoveMatrixSet(MatrixSet ES)
    {
        mtxSetList.Remove(ES);
        inputReceived = true;
    }

    public void ChangeMatrixSet(MatrixSet ES)
    {
        matrixSet = ES;
        inputReceived = true;
    }

    public override void SetOutput(CalcOutput output)
    {
        calcInput.ChangeOutput(output, this);
        inputReceived = true;
    }

    public void manageText()
    {
        selectedElement = expressions.getSelectedExpr();
        ExpressionBody exprBody = expressions.getSelectedBody();

        if (selectedElement == null || exprBody == null) return;

        if (expressions.selectedNotNull())
        {
            textInput = exprBody.getTextInput();
        }

        if (textInput != null)
        {
            int displayLength = (exprBody.isVariable()) ? rangeDisplayLength : expressionDisplayLength;
            textInput.text = displayText(calcInput.currExpression.tokens, calcInput.index, true, displayLength);
        }

        inputReceived = true;
    }

	void Start () {
		
	}
	
	// Update is called once per frame
	void Update () {
		
	}
}
