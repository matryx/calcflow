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
    /// <summary>
    /// Delets all tokens. Returns a list of variables that no longer appear anywhere in the expressionSet.
    /// </summary>
    /// <returns></returns>
    public override List<string> ClearTokens()
    {
        List<string> toDelete = new List<string>();
        List<string> temp = new List<string>(tokens);

        tokens.Clear();
        foreach (string s in temp)
        {
            if (ExpressionSet.getExpressionSet(this).GetTotalOccurence(s) == 0)
            {
                toDelete.Add(s);
            }
        }

        return toDelete;
    }

    public int GetOccurences(string x)
    {
        int count = 0;

        foreach (string s in tokens)
        {
            if (x == s) count++;
        }

        return count;
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
