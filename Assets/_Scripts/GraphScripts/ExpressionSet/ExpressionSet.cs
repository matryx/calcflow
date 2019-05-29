using System.Collections;
using System.Collections.Generic;
using System.Linq;
using UnityEngine;
using Calcflow.UserStatistics;

[System.Serializable]
public class ExpressionSet
{


    public Dictionary<string, Expression> expressions;
    public Dictionary<string, RangePair> ranges;
    public Dictionary<string, bool> expValidity = new Dictionary<string, bool>();
    public AK.ExpressionSolver solver = new AK.ExpressionSolver();

    // string GetExpression(int i)
    // {
    //     return expressions[i].expression;
    // }

    public void AddExpression(string variable, Expression expression)
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

    public void AddExpression(string variable, List<string> tokens)
    {
        if (expressions.ContainsKey(variable))
        {
            expressions[variable].tokens = tokens;
        }
        else
        {
            expressions.Add(variable, new Expression(tokens));
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
        expressions = new Dictionary<string, Expression>();
        expressions.Add("X", new Expression());
        expressions.Add("Y", new Expression());
        expressions.Add("Z", new Expression());

        ranges = new Dictionary<string, RangePair>();
        AddRange("t");
        AddRange("u");
        AddRange("v");
        AddRange("w");
    }

    public ExpressionSet DeepCopy()
    {
        ExpressionSet newEs = new ExpressionSet();
        newEs.expressions = new Dictionary<string, Expression>();
        foreach (string key in expressions.Keys)
        {
            newEs.expressions.Add(key, new Expression(expressions[key]));
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

        newEs.expressions = new Dictionary<string, Expression>(expressions);
        newEs.ranges = new Dictionary<string, RangePair>((Dictionary<string, RangePair>)ranges);
        newEs.expValidity = new Dictionary<string, bool>(expValidity);

        return newEs;
    }

    internal ExpressionSet(string[] rangeKeys, List<RangePair> rangePairs, string[] ExpressionKeys, List<Expression> ExpressionValues)
    {
        ranges = new Dictionary<string, RangePair>();
        for (int i = 0; i < rangePairs.Count; i++)
        {
            ranges.Add(rangeKeys[i], rangePairs[i]);
        }

        expressions = new Dictionary<string, Expression>();
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
        foreach (string EX in expressions.Keys)
        {
            expressions[EX].compileTokens();
            expValidity[EX.ToString()] = expressions[EX].GenerateAKSolver(solver);
            isValid &= expValidity[EX.ToString()];
        }
        StatisticsTracking.InstantEvent("Expression Value", "Value Updated", new Dictionary<string, object>() { { "valid", isValid } });
        return isValid;
    }

    public bool IsCompiled()
    {
        bool isCompiled = true;
        foreach (string EX in expressions.Keys)
        {
            isCompiled &= (expressions[EX].AKExpression != null);
        }
        return isCompiled;
    }

    public void PrintOut()
    {

        foreach (string ex in expressions.Keys)
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
public class SerializableExpressionSet
{
    public string[] rangeKeys;
    public List<SerializableRangePair> rangePairs = new List<SerializableRangePair>();

    public string[] ExpressionKeys;
    public List<string> ExpressionValues = new List<string>();

    public SerializableExpressionSet(ExpressionSet es)
    {
        rangeKeys = new string[es.ranges.Count];
        es.ranges.Keys.CopyTo(rangeKeys, 0);
        foreach (string key in rangeKeys)
        {
            rangePairs.Add(new SerializableRangePair(es.ranges[key]));
        }
        ExpressionKeys = new string[es.expressions.Count];
        es.expressions.Keys.CopyTo(ExpressionKeys, 0);
        foreach (string key in ExpressionKeys)
        {
            ExpressionValues.Add(es.expressions[key].rawText);
        }
    }

    public ExpressionSet ConvertToExpressionSet()
    {
        return new ExpressionSet(rangeKeys, DeserializeRangePairs(), ExpressionKeys, DeserializeExpression());
    }

    private List<RangePair> DeserializeRangePairs()
    {
        return rangePairs.Select(x => x.Deserialize()).ToList();
    }
    private List<Expression> DeserializeExpression()
    {
        return ExpressionValues.Select(exp => new Expression(ExpressionParser.Parse(exp))).ToList();
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