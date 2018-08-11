using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ParametricExpression : MonoBehaviour, ExpressionTabInterface
{ 
    Expressions expressionsClass;
    ExpressionSet expSet;
    ExpressionActions expActions;

    ParametricManager parametricManager;
    Scroll scroll;

    Dictionary<string, Transform> variables;
    Dictionary<string, Transform> hiddenVariables;

    List<string> varsToDelete;
    List<Transform> expressionsList_UI;
    List<Transform> variableClumps_UI;

    Transform expressionX;
    Transform separator;

    Texture quadShow, quadHide;
    Color grayHide, grayShow;

    float xPos = 1.2f;
    bool initialized = false;
    bool deleteVar = false;
    bool destroyCalled = false;
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
            expSet = new ExpressionSet();
            expressionsList_UI = new List<Transform>();
            variableClumps_UI = new List<Transform>();
            variables = new Dictionary<string, Transform>();
            hiddenVariables = new Dictionary<string, Transform>();
            varsToDelete = new List<string>();
            expActions = transform.GetChild(0).GetChild(0).GetChild(0).GetComponentInChildren<ExpressionActions>();

            parametricManager = ParametricManager._instance;
            scroll = expressionsClass.GetScroll(Expressions.ExpressionType.PARAMET);

            InitializeColors();
            SetupUI();

            initialized = true;
        }
    }

    private void InitializeColors()
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
        SetActiveStatus(false);
    }

    public void EnableExpression_UI()
    {
        SetActiveStatus(true);
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

        foreach (KeyValuePair<string, Transform> t in variables)
        {
            t.Value.Find("Min").Find("Button_Input").GetComponent<HighlightOnRaycast>().setDefaultColor(col);
            t.Value.Find("Max").Find("Button_Input").GetComponent<HighlightOnRaycast>().setDefaultColor(col);
        }
    }

    void SetElementQuadTex(Texture tex)
    {
        foreach (KeyValuePair<string, Transform> t in variables)
        {
            t.Value.GetChild(0).Find("Quad").GetComponent<Renderer>().material.mainTexture = tex;
        }
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

        foreach (KeyValuePair<string, Transform> t in variables)
        {
            foreach (Transform child in t.Value)
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
    }

    public void DisableActionButtons_UI()
    {
        expActions.DisableButtons();
    }

    public void SetActiveStatus(bool status)
    {
        isActive = status;
    }

    public bool GetActiveStatus()
    {
        return isActive;
    }

    public Scroll GetScroll()
    {
        return scroll;
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
        return expSet;
    }

    void SetupUI()
    {
        ExpressionSet expressionSet = expSet;

        if (!parametricManager) parametricManager = ParametricManager._instance;
        parametricManager.AddExpressionSet(expressionSet);

        SetupComponents();

        GameObject sep = Instantiate(Resources.Load("Expressions/Separator", typeof(GameObject))) as GameObject;
        SetSeparator(sep.transform);

        expressionsClass.AddExpr(transform);
    }

    void SetupComponents()
    {
        foreach (Transform child in transform)
        {
            if (child.name == "ExpressionSet")
            {
                foreach (Transform gchild in child)
                {
                    if (gchild.name == "Button_Xinput")
                    {
                        SetExpressionX(gchild);
                    }

                    expressionsList_UI.Add(gchild);
                    gchild.GetComponentInChildren<ExpressionBody>().SetManager(ParametricManager._instance);
                    gchild.GetComponentInChildren<ExpressionBody>().SetExpressionParent(transform);
                }
            }
        }
    }

    public List<Transform> GetAllComponents()
    {
        List<Transform> allComponents = new List<Transform>();
        allComponents.AddRange(expressionsList_UI);
        allComponents.AddRange(variables.Values);
        allComponents.Add(separator);

        return allComponents;
    }

    public void AddVariable(string varName, Transform varValue)
    {
        if (hiddenVariables.ContainsKey(varName))
        {
            Transform temp = hiddenVariables[varName];
            temp.gameObject.SetActive(true);
            varValue = temp;
            hiddenVariables.Remove(varName);
        }

        if (variables.Count % 2 == 0)
        {
            AddNewVariableClump(varValue);
        }
        else
        {
            AddToVarClump(varValue);
        }

        variables.Add(varName, varValue);
    }

    public void DeleteExpressionFromScroll()
    {
        scroll.DeleteObjects(expressionsList_UI);
        variableClumps_UI.Add(separator);
        scroll.DeleteObjects(variableClumps_UI);
    }

    public void DeleteVariable(List<string> vars)
    {
        varsToDelete = vars;
        deleteVar = true;
    }

    void AddToVarClump(Transform var)
    {
        var.SetParent(variableClumps_UI[variableClumps_UI.Count - 1]);
        var.localPosition = new Vector3(xPos, 0, 0);
        var.localScale = Vector3.one;
        var.localEulerAngles = Vector3.zero;
        var.gameObject.SetActive(true);
    }

    void AddNewVariableClump(Transform var)
    {
        int lastComponentInd = (variableClumps_UI.Count > 0) ?
                           scroll.GetIndex(variableClumps_UI[variableClumps_UI.Count - 1]) : scroll.GetIndex(expressionsList_UI[2]);

        Transform newVarClump = new GameObject().transform;
        newVarClump.name = "Var Clump";
        newVarClump.localScale = Vector3.one;
        newVarClump.localPosition = Vector3.zero;
        newVarClump.localEulerAngles = Vector3.zero;
        scroll.AddToScroll(null, newVarClump, lastComponentInd + 1);
        variableClumps_UI.Add(newVarClump);

        var.SetParent(newVarClump);
        var.localPosition = new Vector3(-xPos, 0, 0);
        var.localScale = Vector3.one;
        var.localEulerAngles = Vector3.zero;
        var.gameObject.SetActive(true);
    }

    IEnumerator MoveTo(Transform obj, Vector3 start, Vector3 end, float overTime)
    {
        float startTime = Time.time;

        while (Time.time < startTime + overTime)
        {
            if (obj == null) break;
            obj.localPosition = Vector3.Lerp(start, end, (Time.time - startTime) / overTime);
            yield return null;
        }

        if (obj != null) obj.localPosition = end;
    }

    void Update()
    {
        if (deleteVar)
        {
            HideVariables();
            return;
        }

        if (destroyCalled)
        {
            RearrangeVariables();
            DestroyEmptyClumps();
            destroyCalled = false;
        }
    }

    void HideVariables()
    {
        foreach (string s in varsToDelete)
        {
            if (variables.ContainsKey(s))
            {
                Transform temp = variables[s];
                temp.gameObject.SetActive(false);
                temp.SetParent(null);
                hiddenVariables.Add(s, temp);
                variables.Remove(s);

                destroyCalled = true;
                deleteVar = false;
            }
        }
    }

    void RearrangeVariables()
    {
        bool noMoreSlots = false;

        for (int i = 0; i < variableClumps_UI.Count; i++)
        {
            if (noMoreSlots) break;

            Transform currSlot = variableClumps_UI[i];

            if (currSlot.childCount < 2)
            {
                if (currSlot.childCount == 1)
                {
                    StartCoroutine(MoveTo(currSlot.GetChild(0), currSlot.GetChild(0).localPosition, new Vector3(-xPos, 0, 0), 0.3f));
                }
                else
                {
                    noMoreSlots = FindNextSlot(currSlot, i, 0, -xPos, false);
                }

                noMoreSlots = FindNextSlot(currSlot, i, 1, xPos, true);
            }
        }
    }

    bool FindNextSlot(Transform currClump, int currClumpIndex, int childIndex, float xpos, bool checkNoSlot)
    {
        for (int ni = currClumpIndex + 1; ni < variableClumps_UI.Count; ni++)
        {
            Transform nSlot = variableClumps_UI[ni];

            if (nSlot.childCount > 0)
            {
                nSlot.GetChild(0).SetParent(currClump);
                StartCoroutine(MoveTo(currClump.GetChild(childIndex), currClump.GetChild(childIndex).localPosition, new Vector3(xpos, 0, 0), 0.3f));
                return false;
            }

            //only want to do check when finding a slot for second var spot
            if (checkNoSlot && ni == variableClumps_UI.Count - 1) return true;
        }

        return false;
    }

    void DestroyEmptyClumps()
    {
        int removeFrom = 0;
        bool remove = false;

        for (int ind = 0; ind < variableClumps_UI.Count; ind++)
        {
            if (variableClumps_UI[ind].childCount == 0)
            {
                removeFrom = ind;
                remove = true;
                break;
            }
        }

        List<Transform> emptyClumps = new List<Transform>();

        if (remove)
        {
            emptyClumps = variableClumps_UI.GetRange(removeFrom, variableClumps_UI.Count - removeFrom);
            variableClumps_UI.RemoveRange(removeFrom, variableClumps_UI.Count - removeFrom);
            scroll.DeleteObjects(emptyClumps);
        }
    }
}
