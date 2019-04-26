/******************************************
* Management of scene interactions with the 
* coordinate system
******************************************/

using UnityEngine;
using System;
using System.Collections;
using System.Collections.Generic;

public class CoordinateSystemInteraction : MonoBehaviour {

    public enum InteractionMode:int { Create = 0, Modify, Operate, Clear, Function3D };
    public InteractionMode interactionMode;

    public enum OperateMode:int { Addition = 0, CrossProduct };
    public OperateMode opMode;

    public MathBox_Vector vectorPrefab;
    public Transform coordSystem;
    public Transform origin;

    private List<MathBox_Vector> vector_list;
    public List<MathBox_Vector> usercreated_list;
    public List<MathBox_Vector> selected_vectors;
    //private Stack<MathBox_Vector> selected_vectors;
    private MathBox_Vector last_vector
    {
        get
        {
            return vector_list[vector_list.Count - 1];
        }
    }

    private Plotter3D surface;

    public GameObject CARTESIAN, SPHERICAL, CYLINDRICAL;
    public enum CoordType:int { Cartesian = 0, Spherical, Cylindrical };
    public static CoordType coordType = CoordType.Cartesian;

    public Plotter3D surfacePrefab;

    public void SetInteractionMode(InteractionMode mode)
    {
        interactionMode = mode;
    }
    public void SetOpMode(OperateMode mode)
    {
        opMode = mode;
    }
    public void SwitchCoordSystem()
    {
        int currType = (int)coordType;
        currType++;
        currType = currType % 3;
        if (Enum.IsDefined(typeof(CoordType), currType))
        {
            coordType = (CoordType)currType;
        }
    }

    // Use this for initialization
    void Start () {
        vector_list = new List<MathBox_Vector>();
        usercreated_list = new List<MathBox_Vector>();
        selected_vectors = new List<MathBox_Vector>();
	}
	
	// Update is called once per frame
	void Update () {
        switch (coordType)
        {
            case CoordType.Cartesian:
                CARTESIAN.SetActive(true);
                SPHERICAL.SetActive(false);
                CYLINDRICAL.SetActive(false);
                break;
            case CoordType.Spherical:
                CARTESIAN.SetActive(false);
                SPHERICAL.SetActive(true);
                CYLINDRICAL.SetActive(false);
                break;
            case CoordType.Cylindrical:
                CARTESIAN.SetActive(false);
                SPHERICAL.SetActive(false);
                CYLINDRICAL.SetActive(true);
                break;
        }
	}

    public MathBox_Vector CreateVector(Vector3 position)
    {
        MathBox_Vector vector = Instantiate(vectorPrefab);
        vector.transform.SetParent(coordSystem, false);
        vector_list.Add(vector);
        usercreated_list.Add(vector);
        vector.SetPosition(origin.position, position);
        return vector;
    }

    public void SelectVector(MathBox_Vector selected)
    {
        selected.select();
        if (selected_vectors.Contains(selected))
        {
            selected_vectors.Remove(selected);
        }
        else
        {
            selected_vectors.Add(selected);
        }
    }

    public void AddSelected()
    {
        MathBox_Vector v1, v2;
        if((v2 = LastSelection()) != null)
        {
            if((v1 = LastSelection()) != null)
            {
                VectorAddition(v1, v2);
            }
        }
    }

    public void CrossSelected()
    {
        MathBox_Vector v1, v2;
        if ((v2 = LastSelection()) != null)
        {
            if ((v1 = LastSelection()) != null)
            {
                VectorCross(v1, v2);
            }
        }
    }

    public void ClearScene()
    {
        foreach(MathBox_Vector v in vector_list)
        {
            v.ClearVector();
        }
        vector_list.Clear();
        usercreated_list.Clear();
        selected_vectors.Clear();
    }

    MathBox_Vector LastSelection()
    {
        if (selected_vectors.Count != 0)
        {
            MathBox_Vector last = selected_vectors[selected_vectors.Count - 1];
            selected_vectors.RemoveAt(selected_vectors.Count - 1);
            last.select();
            return last;
        }
        else
        {
            return null;
        }
    }

    void VectorAddition(MathBox_Vector v1, MathBox_Vector v2)
    {
        //operand1
        Vector3 start = v1.vectorInfo;
        MathBox_Vector vector = Instantiate(vectorPrefab);
        vector.SetVectorType(MathBox_Vector.VectorType.Addition_operand1, v1, v2);
        vector.transform.SetParent(coordSystem, false);
        vector_list.Add(vector);

        //operand2
        start = v2.vectorInfo;
        vector = Instantiate(vectorPrefab);
        vector.SetVectorType(MathBox_Vector.VectorType.Addition_operand2, v1, v2);
        vector.transform.SetParent(coordSystem, false);
        vector_list.Add(vector);

        //result
        start = origin.position;
        vector = Instantiate(vectorPrefab);
        vector.SetVectorType(MathBox_Vector.VectorType.Addition, v1, v2);
        vector.transform.SetParent(coordSystem, false);
        vector_list.Add(vector);
    }

    void VectorCross(MathBox_Vector v1, MathBox_Vector v2)
    {
        Vector3 start = origin.position;
        //Vector3 end = Vector3.Cross(v1.vectorInfo, v2.vectorInfo);
        MathBox_Vector vector = Instantiate(vectorPrefab);
        vector.SetVectorType(MathBox_Vector.VectorType.CrossProduct, v1, v2);
        vector.transform.SetParent(coordSystem, false);
        vector_list.Add(vector);
    }

    public Plotter3D CreateSurface()
    {
        if(surface != null)
        {
            return surface;
        }
        surface = (Plotter3D)Instantiate(surfacePrefab, Vector3.zero, Quaternion.identity);
        surface.Sampling3D(MyFn1, 0f,5f,0f,5f,0.1f);
        surface.DrawMesh();
        return surface;
    }

    public void ClearSurface()
    {
        surface.Delete();
        surface = null;
    }

    float MyFn1(float x, float z)
    {
        return Mathf.Pow(Mathf.Cos(x), 3) + Mathf.Pow(Mathf.Cos(z), 3) + 1f;
    }
}
