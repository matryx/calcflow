using UnityEngine;
using System.Collections;
using System.Collections.Generic;


public class Interaction : MonoBehaviour {

    // Create: Create and/or delete vectors
    // Edit: Translate or change orientation 
    // Operate: select and perform operation on vectors
    enum GameMode { Create, Edit, Operate};

    public Transform origin;

    public Arrow arrowType;
    public GameObject parent;

    public static int mode;
    public List<Arrow> ourVectors;
    public List<Arrow> selected;
    public List<Matrix4x4> MatrixOps;
    private static int grips;
    public TextMesh area;
    public List<TextMesh> areas;
	public GameObject viveCamera;
	private Arrow ourVector
	{
		get
		{
			return ourVectors[ourVectors.Count - 1];
		}
	}

    /* Move coordinate system change from MakingVector here */
    public TextMesh options;
    private TextMesh gamemodetext, operationtext, coordtext;
    public GameObject GRID, SPHERICAL, CYLINDRICAL;

	// Use this for initialization
	void Start () {
        //ourVector = Instantiate(vectorType);
        mode = 0;

        gamemodetext = Instantiate(options);
        gamemodetext.transform.rotation = Quaternion.Euler(0, 90, 0);
        gamemodetext.transform.position = new Vector3(10, 8, 7);

        operationtext = Instantiate(options);
        operationtext.transform.rotation = Quaternion.Euler(0, 90, 0);
        operationtext.transform.position = new Vector3(10, 6, 7);

        coordtext = Instantiate(options);
        coordtext.transform.rotation = Quaternion.Euler(0, 90, 0);
        coordtext.transform.position = new Vector3(10, 4, 7);
        coordtext.text = "Cartesian";
    }

    // Update is called once per frame
    void Update() {
        gamemodetext.text = MakinVectors.mode.ToString();
        if (MakinVectors.mode == MakinVectors.GameMode.Operate)
            operationtext.text = MakinVectors.operation.ToString();
        else
            operationtext.text = "";

        if (MakinVectors.toggle == MakinVectors.Cartesian)
        {
            // Display "Cartesian"
            coordtext.text = "Cartesian";
            GRID.SetActive(true);
            SPHERICAL.SetActive(false);
            CYLINDRICAL.SetActive(false);
        }
        //else
        //	coordtext.text = "";

        if (MakinVectors.toggle == MakinVectors.Spherical)
        {
            // Display "Spherical"
            coordtext.text = "Spherical";
            GRID.SetActive(false);
            SPHERICAL.SetActive(true);
            CYLINDRICAL.SetActive(false);
        }
        //else
        //	coordtext.text = "";

        if (MakinVectors.toggle == MakinVectors.Cylindrical)
        {
            // Display "Cylindrical"
            coordtext.text = "Cylindrical";
            GRID.SetActive(false);
            SPHERICAL.SetActive(false);
            CYLINDRICAL.SetActive(true);
        }
    }

    public void getAGrip()
    {
		Debug.Log (grips);
        if(grips > 0)
        {
            Debug.Log("Arrows should be cleared");
            clearArrows();
        }
        grips++;
    }

    public void loseAGrip()
    {
        grips--;
    }

    public Arrow createVector(Vector4 position)
    {
        Debug.LogWarning("Interaction::createVector()");
        Arrow arrowObj = Instantiate(arrowType);
        arrowObj.transform.parent = parent.transform;
        ourVectors.Add(arrowObj);
        ourVector.setPoints(parent.transform.position, new Vector3(position.x, position.y, position.z));
		ourVector.setCamRotation (viveCamera);
        return ourVector;
    }

    void addVectors(Arrow first, Arrow second)
    {
        //Vector4 start = first.getInfo();
        //Vector4 end = start + second.getInfo();
        Arrow arrow = Instantiate(arrowType);
        arrow.setResultVec(true, Arrow.ResultType.Addition, first, second);
        arrow.transform.parent = parent.transform;
        ourVectors.Add(arrow);
        //arrow.setPoints(start, start);
        //arrow.lerpPoints(start, end);
        arrow.setCamRotation(viveCamera);

        //start = second.getInfo();
        arrow = Instantiate(arrowType);
        arrow.setResultVec(true, Arrow.ResultType.Addition_helper1, first, second);
        arrow.transform.parent = parent.transform;
        ourVectors.Add(arrow);
        //ourVector.setPoints(start, start);
        //ourVector.lerpPoints(start, end);
        arrow.setCamRotation(viveCamera);

        arrow = Instantiate(arrowType);
        arrow.transform.parent = parent.transform;
        arrow.setResultVec(true, Arrow.ResultType.Addition_helper2, first, second);
        ourVectors.Add(arrow);
        //arrow.lerpPoints(Vector4.zero, end);
        //arrow.lerpPoints(parent.transform.position, end);
        arrow.setCamRotation(viveCamera);

        //Refactored into code above
        //ourVectors.Add(Instantiate(arrowType));
        //ourVectors[ourVectors.Count - 1].setPoints(first.getInfo(), first.getInfo());
        //ourVectors[ourVectors.Count - 1].lerpPoints(first.getInfo(), first.getInfo() + second);
        //ourVectors.Add(Instantiate(arrowType));
        //ourVectors[ourVectors.Count - 1].setPoints(Vector4.zero, first.getInfo());
        //ourVectors[ourVectors.Count - 1].add(second);
    }
    
    Matrix4x4 createScaleMatrix(float factor)
    {

        Matrix4x4 result = Matrix4x4.identity;
        return Matrix4x4.Scale( new Vector3(factor, factor, factor) );
    }

    Matrix4x4 MatrixProduct(Matrix4x4 first, Matrix4x4 second)
    {
        return first * second;

    }

    void clearArrows()
    {
        foreach(Arrow vector in ourVectors)
        {
            Destroy(vector.gameObject);
			vector.removeShip ();
        }
        ourVectors.Clear();
    }

    public void selectVector(Arrow clickedArrow)
    {
        Debug.LogWarning("Interaction::selectVector()");
        if(!selected.Contains(clickedArrow))
        {
            selected.Add(clickedArrow);
        }
        else
        {
            selected.Remove(clickedArrow);
        }
        clickedArrow.select();

    }

    public void addSelected()
    {
        Debug.LogWarning("Interaction::addSelected()");
        addVectors(selected[0], selected[1]);
        selected[0].select();
        selected[1].select();
        selected.Clear();
    }

    Matrix4x4 collapseMatrices()
    {
        Matrix4x4 result = Matrix4x4.identity;

        for (int i = 0; i < MatrixOps.Count; i++)
        {
            result = MatrixProduct(result, MatrixOps[i]);
        }

        MatrixOps.Clear();

        return result;

    }

    void CrossProduct(Arrow a, Arrow b)
    {
        ourVectors.Add(Instantiate(arrowType));
        // ourVectors[ourVectors.Count - 1].setPoints(a.getStart(), cross);
        ourVector.setResultVec(true, Arrow.ResultType.CrossProduct, a, b);
        //ourVectors[ourVectors.Count - 1].lerpPoints(a.getStart(), cross);
        ourVectors[ourVectors.Count - 1].setCamRotation(viveCamera);


    }

    public void crossSelected()
    {
        CrossProduct(selected[0], selected[1]);
        selected[0].select();
        selected[1].select();
        selected.Clear();
    }

    public void dotSelected()
    {
        DotProduct(selected[0], selected[1]);
        selected[0].select();
        selected[1].select();
        selected.Clear();
    }

    void DotProduct(Arrow first, Arrow second)
    {
        Arrow arrow = Instantiate(arrowType);
        ourVectors.Add(arrow);
        arrow.setPoints(first.getInfo(), first.getInfo());
        arrow.lerpPoints(first.getInfo(), first.getInfo() + second.getInfo());

        arrow = Instantiate(arrowType);
        ourVectors.Add(arrow);
        arrow.setPoints(second.getInfo(), second.getInfo());
        arrow.lerpPoints(second.getInfo(), first.getInfo() + second.getInfo());

        TextMesh newArea = Instantiate(area);
        Vector4 finalspot = first.getInfo() + second.getInfo();
        float dotProduct = Vector4.Dot(first.getInfo(), second.getInfo());
        newArea.text = dotProduct.ToString();
        newArea.characterSize *= dotProduct;
        newArea.transform.position = finalspot / 2 - new Vector4(.08f, 0.02f, 0);
        newArea.transform.rotation *= Quaternion.Euler(90 - Vector3.Angle(Vector3.right, finalspot), 90 - Vector3.Angle(Vector3.up, finalspot), 90 - Vector3.Angle(Vector3.forward, finalspot));
        areas.Add(newArea);
    }

    Matrix4x4 makeRotateX(float angle)
    {
        Matrix4x4 result = Matrix4x4.identity;

        result.m11 = Mathf.Cos(angle);
        result.m12 = -1 * Mathf.Sin(angle);
        result.m21 = Mathf.Sin(angle);
        result.m22 = Mathf.Cos(angle);

        return result;

 
    }

    Matrix4x4 makeRotateY(float angle)
    {
        Matrix4x4 result = Matrix4x4.identity;

        result.m00 = Mathf.Cos(angle);
        result.m20 = -1 * Mathf.Sin(angle);
        result.m02 = Mathf.Sin(angle);
        result.m22 = Mathf.Cos(angle);

        return result;


    }
    Matrix4x4 makeRotateZ(float angle)
    {
        Matrix4x4 result = Matrix4x4.identity;

        result.m00 = Mathf.Cos(angle);
        result.m01 = -1 * Mathf.Sin(angle);
        result.m10 = Mathf.Sin(angle);
        result.m11 = Mathf.Cos(angle);

        return result;


    }

    void matrixVectorTransformation(Matrix4x4 matrix, Arrow vector)
    {

        Vector4 start = vector.getStart();
        Vector4 newEnd = matrix.MultiplyVector(vector.getInfo());

        vector.lerpPoints(start, newEnd);




    }
}
