using System.Collections;
using System.Collections.Generic;
using System.Linq;
using UnityEngine;

public class PtSet : MonoBehaviour {
	public enum PtOptions
    {
        pt1, pt2, pt3
    }

	public Dictionary<string, PtCoord> ptCoords;
    public Dictionary<string, bool> expValidity = new Dictionary<string, bool>();
    public AK.ExpressionSolver solver = new AK.ExpressionSolver();

	public void AddPtCoord(string variable, PtCoord ptCoord)
    {
        if (ptCoord != null)
        {
            if (ptCoords.ContainsKey(variable))
            {
                ptCoords[variable] = ptCoord;
            }
            else
            {
                ptCoords.Add(variable, ptCoord);
            }
        }
    }
    public void AddPtCoord(string variable, AxisCoord x, AxisCoord y, AxisCoord z)
    {
        PtCoord ptCoord = new PtCoord(x, y, z);
        AddPtCoord(variable, ptCoord);
    }
    public void AddPtCoord(string variable, List<string> xTokens, List<string> yTokens, List<string> zTokens)
    {
        AxisCoord x = new AxisCoord(xTokens);
        AxisCoord y = new AxisCoord(yTokens);
        AxisCoord z = new AxisCoord(zTokens);
        AddPtCoord(variable, x, y, z);
    }
    public void AddPtCoord(string variable)
    {
        AddPtCoord(variable, new List<string>(), new List<string>(), new List<string>());
    }

    public void RemovePtCoord(string variable)
    {
        if (ptCoords.ContainsKey(variable))
        {
            ptCoords.Remove(variable);
        }
    }

	public PtSet()
	{
		ptCoords = new Dictionary<string, PtCoord>();
        AddPtCoord("pt1");
        AddPtCoord("pt2");
        AddPtCoord("pt3");
	}

	public PtSet DeepCopy()
	{
		PtSet newPs = new PtSet();

		newPs.ptCoords = new Dictionary<string, PtCoord>();
        foreach (string key in ptCoords.Keys)
        {
            newPs.ptCoords.Add(key, new PtCoord(ptCoords[key]));
        }

        newPs.expValidity = new Dictionary<string, bool>(expValidity);

        return newPs;
	}

	public PtSet ShallowCopy()
	{
		PtSet newPs = new PtSet();
        newPs.ptCoords = new Dictionary<string, PtCoord>((Dictionary<string, PtCoord>)ptCoords);
        newPs.expValidity = new Dictionary<string, bool>(expValidity);

        return newPs;
	}

	internal PtSet(string[] ptKeys, List<PtCoord> pts)
	{
		ptCoords = new Dictionary<string, PtCoord>();
        for (int i = 0; i < pts.Count; i++)
        {
            ptCoords.Add(ptKeys[i], pts[i]);
        }
	}

	public bool CompileAll()
	{
		bool isValid = true;
		foreach (string PO in ptCoords.Keys)
        {
            solver.SetGlobalVariable(PO, -666);
            ptCoords[PO].X.compileTokens();
            expValidity[PO] = ptCoords[PO].X.GenerateAKSolver(solver);
            ptCoords[PO].Y.compileTokens();
            expValidity[PO] &= ptCoords[PO].Y.GenerateAKSolver(solver);
            ptCoords[PO].Z.compileTokens();
            expValidity[PO] &= ptCoords[PO].Z.GenerateAKSolver(solver);
            isValid &= expValidity[PO];
        }
		return isValid;
	}

	public void PrintOut()
	{
		foreach (string po in ptCoords.Keys)
        {
            Debug.Log(po);
            ptCoords[po].X.PrintOut();
            ptCoords[po].Y.PrintOut();
            ptCoords[po].Z.PrintOut();
        }
	}

	void SaveToFile()
    {

    }
}

[System.Serializable]
public class PtCoord
{
    public AxisCoord X;
    public AxisCoord Y;
    public AxisCoord Z;

    public PtCoord(AxisCoord x, AxisCoord y, AxisCoord z)
    {
        X = x;
        Y = y;
        Z = z;
    }

    public PtCoord(PtCoord toCopy)
    {
        X = new AxisCoord(toCopy.X);
        Y = new AxisCoord(toCopy.Y);
        Z = new AxisCoord(toCopy.Z);
    }
}

[System.Serializable]
public class AxisCoord : CalcOutput
{
    float val;

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

    public AxisCoord(AxisCoord toCopy)
    {
        this.rawText = toCopy.rawText;
        this.tokens = new List<string>(toCopy.tokens);
    }

    public AxisCoord(List<string> tokens)
    {
        rawText = "";
        this.tokens = tokens;
    }

    public AxisCoord(float numVal) {
        this.val = numVal;
    }
    public AxisCoord()
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
public class SerializablePtCoord
{
    public SerializableAxisCoord X;
    public SerializableAxisCoord Y;
    public SerializableAxisCoord Z;

    public SerializablePtCoord(PtCoord ptCoord)
    {
        X = new SerializableAxisCoord(ptCoord.X);
        Y = new SerializableAxisCoord(ptCoord.Y);
        Z = new SerializableAxisCoord(ptCoord.Z);
    }

    public PtCoord Deserialize()
    {
        return new PtCoord(X.Deserialize(), Y.Deserialize(), Z.Deserialize());
    }
}

[System.Serializable]
public class SerializableAxisCoord
{
    public string rawText;

    public SerializableAxisCoord(AxisCoord axisCoord)
    {
        this.rawText = axisCoord.rawText;
    }

    public AxisCoord Deserialize()
    {
        AxisCoord ac = new AxisCoord(ExpressionParser.Parse(rawText));
        return ac;
    }
}