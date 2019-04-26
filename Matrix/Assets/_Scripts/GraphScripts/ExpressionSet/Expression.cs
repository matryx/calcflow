using System.Collections;
using System.Collections.Generic;
using UnityEngine;


[System.Serializable]
public class Expression : CalcOutput
{
    public string expression
    {
        get
        {
            return rawText;
        }
    }

    public Expression()
    {
        rawText = "";
        tokens = new List<string>();
    }

    public Expression(List<string> tokens)
    {
        rawText = "";
        this.tokens = tokens;
    }

    public Expression(Expression toCopy)
    {
        rawText = toCopy.rawText;
        tokens = new List<string>(toCopy.tokens);
    }
}
