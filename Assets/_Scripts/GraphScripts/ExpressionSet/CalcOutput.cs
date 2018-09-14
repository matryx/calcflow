using System.Collections;
using System.Collections.Generic;
using UnityEngine;


[System.Serializable]
public abstract class CalcOutput
{
    public List<string> tokens;
    public string rawText;
    public AK.Expression AKExpression;

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

    public virtual bool IsValid()
    {
        try
        {
            AK.ValidityChecker.CheckValidity(AK.SolverTools.RemoveWhiteSpace(rawText));
            return true;
        }
        catch (AK.ESSyntaxErrorException ex)
        {
            return false;
        }
    }

    public virtual bool GenerateAKSolver(AK.ExpressionSolver solver)
    {
        try
        {
            AKExpression = solver.SymbolicateExpression(rawText);
        }
        catch (AK.ESSyntaxErrorException ex)
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
