using System.Collections;
using System.Collections.Generic;
using System.Linq;
using UnityEngine;

[System.Serializable]
public class ExpressionSet
{
    public enum ExpOptions
    {
        X, Y, Z
    }

    public Dictionary<ExpOptions, Expression> expressions;
    public Dictionary<string, RangePair> ranges;
    public Dictionary<string, RangePair> hiddenRanges;
    public Dictionary<string, bool> expValidity = new Dictionary<string, bool>();
    public AK.ExpressionSolver solver = new AK.ExpressionSolver();

    string GetExpression(int i)
    {
        return expressions[(ExpOptions)i].expression;
    }

    public int GetTotalOccurence(string target)
    {
        int totalCount = 0;

        foreach(KeyValuePair<ExpOptions, Expression> entry in expressions)
        {
            totalCount += entry.Value.GetOccurences(target);
        }

        return totalCount;
    }

    public void AddExpression(ExpOptions variable, Expression expression)
    {
        if (expression != null)
        {
            if (expressions.ContainsKey(variable))
            {
                expressions[variable] = expression;
            }
            else
            {
                expressions.Add(variable, expression);
            }
        }
    }

    //TODO: create function thta moves range from ranges to hiddenRanges and vice veersa

    public void AddExpression(ExpOptions variable, List<string> tokens)
    {
        if (expressions.ContainsKey(variable))
        {
            expressions[variable].tokens = tokens;
        }
        else
        {
            expressions.Add(variable, new Expression(tokens, this));
        }
    }

    public void AddRange(string variable, RangePair rangePair)
    {
        if (rangePair != null)
        {
            if (ranges.ContainsKey(variable))
            {
                ranges[variable] = rangePair;
            }
            else
            {
                ranges.Add(variable, rangePair);
            }
        }
    }
    public void AddRange(string variable, Range rangeMin, Range rangeMax)
    {
        RangePair rangePair = new RangePair(rangeMin, rangeMax);
        AddRange(variable, rangePair);
    }
    public void AddRange(string variable, List<string> minTokens, List<string> maxTokens)
    {
        Range rangeMin = new Range(minTokens);
        Range rangeMax = new Range(maxTokens);
        AddRange(variable, rangeMin, rangeMax);
    }
    public void AddRange(string variable)
    {
        AddRange(variable, new List<string>(), new List<string>());
    }

    public void RemoveRange(string variable)
    {
        if (ranges.ContainsKey(variable))
        {
            ranges.Remove(variable);
        }
    }

    public ExpressionSet()
    {
        expressions = new Dictionary<ExpOptions, Expression>();
        expressions.Add(ExpOptions.X, new Expression(this));
        expressions.Add(ExpOptions.Y, new Expression(this));
        expressions.Add(ExpOptions.Z, new Expression(this));

        //need to change later
        ranges = new Dictionary<string, RangePair>();
    }

    //CREATES EMPTY EXPRESSION SET
    public ExpressionSet(List<string> emptyTokens)
    {
        if (emptyTokens.Count == 0) emptyTokens.Add("0");

        expressions = new Dictionary<ExpOptions, Expression>();
        emptyTokens.Add("x");
        expressions.Add(ExpOptions.X, new Expression(emptyTokens, this));
        emptyTokens.Remove("x");
        expressions.Add(ExpOptions.Y, new Expression(emptyTokens, this));
        expressions.Add(ExpOptions.Z, new Expression(emptyTokens, this));

        ranges = new Dictionary<string, RangePair>();
        ranges.Add("x", new RangePair(new Range(emptyTokens), new Range(emptyTokens)));
        this.CompileAll();
    }

    public ExpressionSet DeepCopy()
    {
        ExpressionSet newEs = new ExpressionSet();
        newEs.expressions = new Dictionary<ExpOptions, Expression>();
        foreach (ExpOptions key in expressions.Keys)
        {
            newEs.expressions.Add(key, new Expression(expressions[key], this));
        }

        newEs.ranges = new Dictionary<string, RangePair>();
        foreach (string key in ranges.Keys)
        {
            newEs.ranges.Add(key, new RangePair(ranges[key]));
        }

        newEs.expValidity = new Dictionary<string, bool>(expValidity);

        return newEs;
    }

    public ExpressionSet ShallowCopy()
    {
        ExpressionSet newEs = new ExpressionSet();

        newEs.expressions = new Dictionary<ExpOptions, Expression>(expressions);
        newEs.ranges = new Dictionary<string, RangePair>((Dictionary<string, RangePair>)ranges);
        newEs.expValidity = new Dictionary<string, bool>(expValidity);

        return newEs;
    }

    internal ExpressionSet (string[] rangeKeys, List<RangePair> rangePairs, ExpOptions[] ExpressionKeys, List<Expression> ExpressionValues)
    {
        ranges = new Dictionary<string, RangePair>();
        for (int i =0; i < rangePairs.Count; i++)
        {
            ranges.Add(rangeKeys[i], rangePairs[i]);
        }

        expressions = new Dictionary<ExpOptions, Expression>();
        for (int i = 0; i < ExpressionValues.Count; i++)
        {
            expressions.Add(ExpressionKeys[i], ExpressionValues[i]);
        }
    }

    public bool CompileAll()
    {
        bool isValid = true;
        foreach (string RO in ranges.Keys)
        {
            solver.SetGlobalVariable(RO, -666);
            ranges[RO].Min.compileTokens();
            expValidity[RO] = ranges[RO].Min.GenerateAKSolver(solver);
            ranges[RO].Max.compileTokens();
            expValidity[RO] &= ranges[RO].Max.GenerateAKSolver(solver);
            isValid &= expValidity[RO];
        }
        foreach (ExpOptions EX in expressions.Keys)
        {
            expressions[EX].compileTokens();
            expValidity[EX.ToString()] = expressions[EX].GenerateAKSolver(solver);
            isValid &= expValidity[EX.ToString()];
        }
        return isValid;
    }

    public void PrintOut()
    {

        foreach (ExpOptions ex in expressions.Keys)
        {
            Debug.Log(ex.ToString());
            expressions[ex].PrintOut();
        }
        foreach (string ro in ranges.Keys)
        {
            Debug.Log(ro);
            ranges[ro].Min.PrintOut();
            ranges[ro].Max.PrintOut();
        }
    }

}

[System.Serializable]
public abstract class CalcOutput
{
    public List<string> tokens;
    public string rawText;
    public AK.Expression AKExpression;
    public ExpressionSet expSet;

    public abstract List<string> ClearTokens();

    public void setExpressionSet(ExpressionSet es)
    {
        expSet = es;
    }

    public void PrintOut()
    {
        Debug.Log("CalcOutput tokens: " + string.Join("", tokens.ToArray()));
    }

    public virtual void compileTokens()
    {
        List<string> equation = new List<string>(tokens);

        int paren = 0;
        if (equation.Count == 0)
        {
            rawText = "0";
            return;
        }
        for (int i = 0; i < equation.Count; i++)
        {
            string curr = equation[i];
            /* counting parens */
            if (isCloseP(curr)) paren--;
            if (hasOpenP(curr)) paren++;

            if (i == 0) continue;

            string last = equation[i - 1];

            /* Situations where we must add multiplication symbols.*/
            if ((isNum(last) && !isSymbol(curr) && !isNum(curr) && !isCloseP(curr))
             || (isVar(last) && !isSymbol(curr) && !isCloseP(curr))
             || (isCloseP(last) && !isSymbol(curr) && !isCloseP(curr)))
            {
                equation.Insert(i++, "*");
            }


        }
        while (paren < 0)
        {
            equation.Insert(0, "(");
            paren++;
        }
        while (paren > 0)
        {
            equation.Add(")");
            paren--;
        }
        rawText = string.Join("", equation.ToArray());
    }

    public virtual bool GenerateAKSolver(AK.ExpressionSolver solver)
    {
        try 
        {
            AKExpression = solver.SymbolicateExpression(rawText);
        }
        catch (System.Exception exception)
        {
            return false;
        }
        return true;
    }

    #region helper Functions
    /// <summary>
    /// checks if string represents a variable.
    /// </summary>
    /// <param name="s"></param>
    /// <returns></returns>
    // Heuristic: checks if the last char is a lowercase letter. 
    //            This way we get 'pi' but not 'cos('
    bool isVar(string s)
    {
        if (s[s.Length - 1] > 96 && s[s.Length - 1] < 123) return true;
        return false;
    }
    /// <summary>
    /// checks if string represents a number.
    /// </summary>
    /// <param name="s"></param>
    /// <returns></returns>
    // Heuristic: checks if the last char is a number. 
    bool isNum(string s)
    {
        if (s[s.Length - 1] == '.') return true;
        if (s[s.Length - 1] > 47 && s[s.Length - 1] < 58) return true;
        return false;
    }
    /// <summary>
    /// checks if string represents a close paren.
    /// </summary>
    /// <param name="s"></param>
    /// <returns></returns>
    bool isCloseP(string s)
    {
        if (s[s.Length - 1] == ')') return true;
        return false;
    }
    /// <summary>
    /// Checks if last char is an open paren. Captures functions and open parens.
    /// </summary>
    /// <param name="s"></param>
    /// <returns></returns>
    bool hasOpenP(string s)
    {
        if (s[s.Length - 1] == '(') return true;
        return false;
    }

    /// <summary>
    /// checks if string represents a mathematical operator.
    /// </summary>
    /// <param name="s"></param>
    /// <returns></returns>
    bool isSymbol(string s)
    {
        if (s[s.Length - 1] == '^') return true;
        if (s[s.Length - 1] > 41 && s[s.Length - 1] < 48) return true;
        return false;
    }
    #endregion
}

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

    public override List<string> ClearTokens()
    {
        List<string> toDelete = new List<string>();
        List<string> temp = new List<string>(tokens);

        tokens.Clear();

        foreach (string s in temp)
        {
            if (expSet.GetTotalOccurence(s) == 0)
            {
                toDelete.Add(s);
            }
        }

        return toDelete;
    }

    public int GetOccurences(string x)
    {
        int count = 0;

        foreach(string s in tokens)
        {
            if (x == s) count++; 
        }

        return count;
    }

    public Expression(ExpressionSet es)
    {
        expSet = es;
        rawText = "";
        tokens = new List<string>();
    }

    public Expression(List<string> tokens, ExpressionSet es)
    {
        expSet = es;
        rawText = "";
        this.tokens = tokens;
    }

    public Expression(Expression toCopy, ExpressionSet es)
    {
        expSet = es;
        rawText = toCopy.rawText;
        tokens = new List<string>(toCopy.tokens);
    }
}

[System.Serializable]
public class RangePair
{
    public Range Min;
    public Range Max;

    public RangePair(Range min, Range max)
    {
        Min = min;
        Max = max;
    }

    public RangePair(RangePair toCopy)
    {
        Min = new Range(toCopy.Min);
        Max = new Range(toCopy.Max);
    }
}

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

[System.Serializable]
public class SerializableExpressionSet 
{
    public string[] rangeKeys;
    public List<SerializableRangePair> rangePairs = new List<SerializableRangePair>();

    public ExpressionSet.ExpOptions[] ExpressionKeys;
    public List<string> ExpressionValues = new List<string>();

    public SerializableExpressionSet(ExpressionSet es)
    {
        rangeKeys = new string[es.ranges.Count];
        es.ranges.Keys.CopyTo(rangeKeys, 0);
        foreach (string key in rangeKeys)
        {
            rangePairs.Add(new SerializableRangePair(es.ranges[key]));
        }
        ExpressionKeys = new ExpressionSet.ExpOptions[es.expressions.Count];
        es.expressions.Keys.CopyTo(ExpressionKeys, 0);
        foreach (ExpressionSet.ExpOptions key in ExpressionKeys)
        {
            ExpressionValues.Add(es.expressions[key].rawText);
        }
    }

    public ExpressionSet ConvertToExpressionSet()
    {
        ExpressionSet es = new ExpressionSet();
        List<RangePair> rps = DeserializeRangePairs(es);
        for (int i=0; i < rps.Count(); i++)
        {
            es.AddRange(rangeKeys[i], rps[i]);
        }
        List<Expression> expl = DeserializeExpression(es);
        for (int i = 0; i < rps.Count(); i++)
        {
            expl[i].setExpressionSet(es);
            es.AddExpression(ExpressionKeys[i], expl[i]);
        }

        return es;
    }

    private List<RangePair> DeserializeRangePairs(ExpressionSet es)
    {
        return rangePairs.Select(x => x.Deserialize()).ToList();
    }
    private List<Expression> DeserializeExpression(ExpressionSet es)
    {
        return ExpressionValues.Select(exp => new Expression(ExpressionParser.Parse(exp), es)).ToList();
    }
}

[System.Serializable]
public class SerializableRangePair
{
    public SerializableRange min;
    public SerializableRange max;

    public SerializableRangePair(RangePair rangePair)
    {
        min = new SerializableRange(rangePair.Min);
        max = new SerializableRange(rangePair.Max);
    }

    public RangePair Deserialize()
    {
        return new RangePair(min.Deserialize(), max.Deserialize());
    }
}

[System.Serializable]
public class SerializableRange
{
    public bool exclusive;
    public string rawText;

    public SerializableRange(Range range)
    {
        exclusive = range.Exclusive;
        rawText = range.rawText;
    }

    public Range Deserialize()
    {
        Range rn = new Range(ExpressionParser.Parse(rawText));
        rn.Exclusive = exclusive;
        return rn;
    }
}