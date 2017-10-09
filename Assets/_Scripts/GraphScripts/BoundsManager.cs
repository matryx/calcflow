using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System.Text.RegularExpressions;



public class BoundsManager : MonoBehaviour {
    private CalcManager calcManager;
    public TMPro.TextMeshPro tParam1, tParam2;
    public TMPro.TextMeshPro uParam1, uParam2;
    public TMPro.TextMeshPro vParam1, vParam2;
    public TMPro.TextMeshPro wParam1, wParam2;

    internal class KeyboardInputResponder : FlexMenu.FlexMenuResponder
    {
        BoundsManager boundsManager;
        internal KeyboardInputResponder(BoundsManager boundsManager)
        {
            this.boundsManager = boundsManager;
        }

        public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
        {
            boundsManager.HandleInput(sender);
        }

        public void Flex_ActionEnd(string name, FlexActionableComponent sender, GameObject collider) { }

    }


    public void Initialize(CalcManager cm)
    {
        responder = new KeyboardInputResponder(this);
        keyboard.RegisterResponder(responder);
        calcManager = cm;
    }

    public void HandleInput(FlexActionableComponent button)
    {
        string[] q = button.name.Split(new string[] { "Paren" }, System.StringSplitOptions.None);
        string param = q[0];
        string index = q[q.Length - 1]; 
        if (index == "1")
        {
            calcManager.expressionSet.ranges[param].Min.Exclusive = !calcManager.expressionSet.ranges[param].Min.Exclusive;
        }
        if (index == "2")
        {
            calcManager.expressionSet.ranges[param].Max.Exclusive = !calcManager.expressionSet.ranges[param].Max.Exclusive;
        }

        calcManager.inputReceived = true;
        UpdateButtonText();
    }

    public FlexMenu keyboard;

    KeyboardInputResponder responder;


    public void UpdateButtonText()
    {
        if (tParam1 != null && tParam2 != null)
        {
            UpdateSingleButtonText(tParam1, tParam2, "t");
        }
        if (uParam1 != null && uParam2 != null)
        {
            UpdateSingleButtonText(uParam1, uParam2, "u");
        }
        if (vParam1 != null && vParam2 != null)
        {
            UpdateSingleButtonText(vParam1, vParam2, "v");
        }
        if (wParam1 != null && wParam2 != null)
        {
            UpdateSingleButtonText(wParam1, wParam2, "w");
        }
    } 


    private void UpdateSingleButtonText(TMPro.TextMeshPro tm1, TMPro.TextMeshPro tm2, string param)
    {
        Range r = calcManager.expressionSet.ranges[param].Min;
        if (r.Exclusive)
        {
            tm1.text = "(";
        }
        else
        {
            tm1.text = "[";
        }
        r = calcManager.expressionSet.ranges[param].Max;
        if (r.Exclusive)
        {
            tm2.text = ")";
        }
        else
        {
            tm2.text = "]";
        }
    }
}
