using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class VectorFieldExpression : MonoBehaviour, ExpressionTabInterface
{
    Expressions expressionsClass;
    ExpressionSet expressionSet;
    ExpressionActions expActions;

    VecFieldManager vectorFieldManager;
    Scroll scroll;

    List<Transform> expressionsList_UI;

    Transform range;
    Transform expressionX;
    Transform separator;

    Texture quadShow, quadHide;
    Color grayHide, grayShow;

    List<string> min;
    List<string> max;

    bool initialized = false;
    bool isActive = true;

    void Awake()
    {
        Initialize();
    }

    public void Initialize()
    {
        if (!initialized)
        {
            expressionsClass = Expressions._instance;
            expressionSet = new ExpressionSet();
            expressionsList_UI = new List<Transform>();

            expActions = transform.GetChild(0).GetChild(0).GetChild(0).GetComponentInChildren<ExpressionActions>();
            vectorFieldManager = VecFieldManager._instance;
            scroll = expressionsClass.GetScroll(Expressions.ExpressionType.VECFIELD);

            min = new List<string> { "-", "9" };
            max = new List<string> { "9" };

            InitializeColors();
            SetupUI();

            initialized = true;
        }
    }

    void InitializeColors()
    {
        quadShow = Resources.Load("Icons/element", typeof(Texture2D)) as Texture;
        quadHide = Resources.Load("Icons/element_gray", typeof(Texture2D)) as Texture;
        ColorUtility.TryParseHtmlString("#9E9E9EFF", out grayShow);
        ColorUtility.TryParseHtmlString("#D4D4D4FF", out grayHide);
    }

    public void DisableExpression_UI()
    {
        SetTextColor(grayHide);
        SetButtonInputColor(grayHide);
        SetElementQuadTex(quadHide);
    }

    public void EnableExpression_UI()
    {
        SetTextColor(Color.black);
        SetElementQuadTex(quadShow);
        SetButtonInputColor(grayShow);
    }

    void SetButtonInputColor(Color col)
    {
        foreach (Transform t in expressionsList_UI)
        {
            t.Find("Button_Input").GetComponent<HighlightOnRaycast>().setDefaultColor(col);
        }

        range.Find("Min").Find("Button_Input").GetComponent<HighlightOnRaycast>().setDefaultColor(col);
        range.Find("Max").Find("Button_Input").GetComponent<HighlightOnRaycast>().setDefaultColor(col);
    }

    void SetElementQuadTex(Texture tex)
    {
        range.GetChild(0).Find("Quad").GetComponent<Renderer>().material.mainTexture = tex;
    }

    void SetTextColor(Color c)
    {
        foreach (Transform t in expressionsList_UI)
        {
            foreach (Transform child in t)
            {
                if (child.GetComponent<TMPro.TextMeshPro>())
                {
                    child.GetComponent<TMPro.TextMeshPro>().color = c;
                }
            }
        }

        foreach (Transform child in range)
        {
            foreach (Transform gchild in child)
            {
                if (gchild.GetComponent<TMPro.TextMeshPro>())
                {
                    gchild.GetComponent<TMPro.TextMeshPro>().color = c;
                }
            }
        }
    }

    public void SetRange(Transform r)
    {
        range = r;
    }

    public void SetExpressionX(Transform e)
    {
        expressionX = e;
    }

    public Transform GetExpressionX()
    {
        return expressionX;
    }

    public void SetSeparator(Transform sep)
    {
        separator = sep;
    }

    public Transform GetSeparator()
    {
        return separator;
    }

    public ExpressionSet GetExpSet()
    {
        return expressionSet;
    }

    void SetupUI()
    {
        if (!vectorFieldManager) vectorFieldManager = VecFieldManager._instance;
        vectorFieldManager.AddExpressionSet(expressionSet);
        SetupComponents();
        SetupVariables();

        GameObject sepVec = Instantiate(Resources.Load("Expressions/Separator", typeof(GameObject))) as GameObject;
        SetSeparator(sepVec.transform);

        expressionsClass.AddExpr(transform);
    }

    void SetupComponents()
    {
        foreach (Transform child in transform.Find("ExpressionSet"))
        {
            if (child.name == "Button_Xinput")
            {
                SetExpressionX(child);
            }

            expressionsList_UI.Add(child);
            child.GetComponentInChildren<ExpressionBody>().SetManager(vectorFieldManager);
            child.GetComponentInChildren<ExpressionBody>().SetExpressionParent(transform);
        }
    }

    void SetupVariables()
    {
        GameObject var = Instantiate(Resources.Load("Expressions/Variable", typeof(GameObject))) as GameObject;
        var.gameObject.SetActive(true);
        var.transform.localScale = Vector3.one;

        var.transform.Find("Min").GetComponentInChildren<ExpressionBody>().SetManager(vectorFieldManager);
        var.transform.Find("Max").GetComponentInChildren<ExpressionBody>().SetManager(vectorFieldManager);
        var.transform.Find("Min").GetComponentInChildren<ExpressionBody>().SetExpressionParent(transform);
        var.transform.Find("Max").GetComponentInChildren<ExpressionBody>().SetExpressionParent(transform);
        var.transform.Find("VariableTitle").Find("Body").GetComponent<ExpressionBody>().SetTitle("t");
        SetRange(var.transform);

        expressionSet.AddRange("t");
        expressionSet.AddRange("x", min, max);
        expressionSet.AddRange("y", min, max);
        expressionSet.AddRange("z", min, max);
    }

    public List<Transform> GetAllComponents()
    {
        List<Transform> allComponents = new List<Transform>();
        allComponents.AddRange(expressionsList_UI);
        allComponents.Add(range);
        allComponents.Add(separator);

        return allComponents;
    }

    public void DisableActionButtons_UI()
    {
        expActions.DisableButtons();
    }

    public bool GetActiveStatus()
    {
        return isActive;
    }

    public void DeleteExpressionFromScroll()
    {
        List<Transform> rest = new List<Transform>();
        rest.Add(range);
        rest.Add(separator);

        scroll.DeleteObjects(expressionsList_UI);
        scroll.DeleteObjects(rest);
    }
    
    void Update() { }
}
