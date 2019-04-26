using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System;
public class Parametrization {

    const int MAX_PARAM = 8;

    List<string> parameter_names;
    Dictionary<int, string> indexedParam;
    Dictionary<string, string> parameter_min;
    Dictionary<string, float> parameterMin;
    Dictionary<string, string> parameter_max;
    Dictionary<string, float> parameterMax;

    string expression1;
    string expression2;
    string expression3;

    AK.ExpressionSolver solver = new AK.ExpressionSolver();
    AK.Expression expr1 = new AK.Expression(), expr2 = new AK.Expression(), expr3 = new AK.Expression();
    //List<Variable> vars = new List<Variable>();
    Dictionary<string, AK.Variable> vars = new Dictionary<string, AK.Variable>();

    const int PARTICLE_COUNT = 1000000;
    int depth;
    int width;

    public void SetExpr1(string expr)
    {
        expression1 = expr;
    }
    public void SetExpr2(string expr)
    {
        expression2 = expr;
    }
    public void SetExpr3(string expr)
    {
        expression3 = expr;
    }
    public void AddParameter(string name)
    {
        parameter_names.Add(name);
    }
    public void SetParameterMin(string name, string minExpr)
    {
        parameter_min[name] = minExpr;
    }
    public void SetParameterMax(string name, string maxExpr)
    {
        parameter_max[name] = maxExpr;
    }
    public void SetParamValue(string name, float ratio)
    {
        AK.Variable var = vars[name];
        var.value = parameterMin[name] + ratio * (parameterMax[name] - parameterMin[name]);
    }

    public void SetupSolver()
    {
        depth = parameter_names.Count;
        width = (int)Mathf.Pow(PARTICLE_COUNT, 1f / (float)depth);

        for(int i = 0; i < depth; i++)
        {
            string name = parameter_names[i];
            indexedParam[i] = name;
            parameterMin[name] = (float)solver.EvaluateExpression(parameter_min[name]);
            parameterMax[name] = (float)solver.EvaluateExpression(parameter_max[name]);

            solver.SetGlobalVariable(name, 0);
        }

        expr1 = solver.SymbolicateExpression(expression1);
        expr2 = solver.SymbolicateExpression(expression2);
        expr3 = solver.SymbolicateExpression(expression3);

        for (int i = 0; i < depth; i++)
        {
            string name = parameter_names[i];
            AK.Variable var = solver.GetGlobalVariable(name);
            //vars.Add(var);
            vars[name] = var;
        }
    }

    List<int[]> samples = new List<int[]>();
    public void SetupSamples()
    {
        bool done = false;
        int[] array = new int[depth];
        while (!done)
        {
            samples.Add(array.Clone() as int[]);

            array[0] += 1;
            for(int i = 0; i < depth; i++)
            {
                if(array[i] > width)
                {
                    if (i + 1 == depth)
                    {
                        done = true;
                        break;
                    }
                    array[i] = 0;
                    array[i + 1] += 1;
                }
            }
        }
    }

    List<Vector3> points = new List<Vector3>();
    public List<Vector3> Evaluate()
    {
        for(int i = 0; i < samples.Count; i++)
        {
            int[] arr = samples[i];
            for(int j = 0; j < depth; j++)
            {
                int val = arr[j];
                string name = indexedParam[j];
                SetParamValue(name, (float)val/width);
            }
            float x = (float)expr1.Evaluate();
            float z = (float)expr2.Evaluate();
            float y = (float)expr3.Evaluate();

            points.Add(new Vector3(x, y, z));
        }
        return points;
    }

    public void Initialize()
    {
        parameter_names = new List<string>();
        indexedParam = new Dictionary<int, string>();
        parameter_min = new Dictionary<string, string>();
        parameterMin = new Dictionary<string, float>();
        parameter_max = new Dictionary<string, string>();
        parameterMax = new Dictionary<string, float>();

        samples = new List<int[]>();
        points = new List<Vector3>();
    }

    public void run()
    {
        SetupSolver();
        SetupSamples();
        List<Vector3> positions = Evaluate();
    }
}
