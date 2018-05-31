using System.Collections;
using System.Collections.Generic;
using System.Linq;
using UnityEngine;
using Calcflow.UserStatistics;

public class MatrixSet 
{   
    // Should contain "A", "x" and "b" as in Ax = b 
	public Dictionary<string, Matrix> matrices;
	public Dictionary<string, bool> expValidity = new Dictionary<string, bool>();
    public AK.ExpressionSolver solver = new AK.ExpressionSolver();

    private static Dictionary<CalcOutput, MatrixSet> matrixSetDict = new Dictionary<CalcOutput, MatrixSet>();

    private void setMatrixSet(CalcOutput calcOutput)
    {
        if (matrixSetDict.ContainsKey(calcOutput))
        {
            matrixSetDict[calcOutput] = this;
        }
        else
        {
            matrixSetDict.Add(calcOutput, this);
        }
    }

    public static MatrixSet getMatrixSet(CalcOutput calcOutput)
    {
        MatrixSet result;
        if (matrixSetDict.TryGetValue(calcOutput, out result))
        {
            return result;
        }
        else
        {
            Debug.Log("<color=red> calcOutput not in any MatrixSet </color>");
            return null;
        }
    }

    private static void removeMatrixSet(CalcOutput calcOutput)
    {
        matrixSetDict.Remove(calcOutput);
    }

    public Element getElement(string mtxName, int x, int y)
    {
        Matrix currMtx;
        if (matrices.TryGetValue(mtxName, out currMtx)) 
        {
            if(currMtx.xDim <= x || currMtx.yDim <= y)
                return null;
            return currMtx.mtx[x,y];
        }
        else
        {
            Debug.Log("<color=red> Matrix does not exist </color>");
            return null;
        }
    }





    public MatrixSet() 
    {
        matrices = new Dictionary<string, Matrix>();
        matrices.Add("A", new Matrix(new Element[3,3]));
        matrices.Add("x", new Matrix(new Element[3,1]));
        matrices.Add("b", new Matrix(new Element[3,1]));
    }

    public MatrixSet(Dictionary<string, Matrix> newDictionary) 
    {
        matrices = newDictionary;
    }

    public MatrixSet DeepCopy() 
    {
        MatrixSet newSet = new MatrixSet();
        newSet.matrices = new Dictionary<string, Matrix>();
        foreach (string key in matrices.Keys) 
        {
            newSet.matrices.Add(key, matrices[key].DeepCopy());
        }
        newSet.expValidity = new Dictionary<string, bool>(expValidity);
        return newSet;
    }

    public MatrixSet ShallowCopy() 
    {
        MatrixSet newSet = new MatrixSet();
        newSet.matrices = this.matrices;
        newSet.expValidity = this.expValidity;
        return newSet;
    }

    public bool CompileAll() 
    {
        bool isValid = true;
        foreach (string key in this.matrices.Keys)
        {
            expValidity[key] = true;
            foreach (Element cell in matrices[key].mtx) {
                cell.compileTokens();
                expValidity[key] &= cell.GenerateAKSolver(solver);
            }
            isValid &= expValidity[key];
        }
        //StatisticsTracking.InstantEvent("Expression Value", "Value Updated", new Dictionary<string, object>() { { "valid", isValid } });
        return isValid;
    }
}

public class Matrix
{
	public int xDim; 
	public int yDim;
	public Element [ , ] mtx; 
	public Matrix(Element[ , ] mtx) {
		this.mtx = mtx;
		this.xDim = mtx.GetLength(0);
		this.yDim = mtx.GetLength(1);
	}

    public Matrix DeepCopy()
    {
        Matrix newMatrix = new Matrix(new Element[xDim,yDim]);
        for (int x = 0; x < xDim; x++) {
            for (int y = 0; y < yDim; y++) {
                newMatrix.mtx[x,y] = new Element(mtx[x,y]);
            }
        }
        return newMatrix;
    }
}

public class Element : CalcOutput
{
    float val;

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

    public Element(Element toCopy)
    {
        this.rawText = toCopy.rawText;
        this.tokens = new List<string>(toCopy.tokens);
    }

    public Element(List<string> tokens)
    {
        rawText = "";
        this.tokens = tokens;
    }

    public Element()
    {
        rawText = "";
        tokens = new List<string>();
        tokens.Add("0");
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

