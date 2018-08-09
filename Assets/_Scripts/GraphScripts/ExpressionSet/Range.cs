using System.Collections;
using System.Collections.Generic;
using UnityEngine;


[System.Serializable]
public class Range : CalcOutput
{
    float val;
    public bool Exclusive = false;

    public override List<string> ClearTokens()
    {
        tokens.Clear();
        return null;
    }

    public float Value
    {
        get
        {
            return val;
        }
    }

    public string expression
    {
        get
        {
            return rawText;
        }
        set
        {
            rawText = value;
        }
    }

    public Range(Range toCopy)
    {
        this.rawText = toCopy.rawText;
        this.tokens = new List<string>(toCopy.tokens);
        this.Exclusive = toCopy.Exclusive;
    }

    public Range(List<string> tokens)
    {
        rawText = "";
        this.tokens = tokens;
    }

    public Range()
    {
        rawText = "";
        tokens = new List<string>();
    }

    public override bool GenerateAKSolver(AK.ExpressionSolver solver)
    {
        bool success = base.GenerateAKSolver(solver);
        if (success)
        {
            val = (float)AKExpression.Evaluate();
        }
        return success;
    }

}
