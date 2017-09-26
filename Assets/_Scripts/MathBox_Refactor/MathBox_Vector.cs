/*************************************
* Visualization of vectors in 3D space
*************************************/

using UnityEngine;
using System.Collections;

public class MathBox_Vector : MonoBehaviour {
    public enum VectorType { UserCreated, Addition, Addition_operand1, Addition_operand2, CrossProduct, VectorField };

    public GameObject tipPrefab;

    private Vector3 startPt, endPt;
    private Vector3 prevPos, targetPos;
    private float? lerpStartTime = null;

    VectorType type = VectorType.UserCreated;
    MathBox_Vector operand1 = null;
    MathBox_Vector operand2 = null;
    bool selected;
    private GameObject arrowTip;

    public InteractionHandler scene;

    public Vector3 vectorInfo
    {
        get
        {
            return endPt - startPt;
        }
    }
    public Vector3 GetStart
    {
        get
        {
            return startPt;
        }
    }
    public Vector3 GetEnd
    {
        get
        {
            return endPt;
        }
    }

    // Vector variables
    float x, y, z; // car
    float rho, theta, phi; // sph
    float r; // cyl
    string display_theta, display_phi;


	// Use this for initialization
	void Start () {
        GetComponent<LineRenderer>().SetPosition(0, startPt);
        GetComponent<LineRenderer>().SetPosition(1, endPt);

        arrowTip = InitArrowTip();

        scene = GameObject.FindObjectOfType<InteractionHandler>();
	}
	
	// Update is called once per frame
	void Update () {
        LineRenderer lr;
        switch (type)
        {
            case VectorType.Addition:
                endPt = operand1.vectorInfo + operand2.vectorInfo;
                ShowVectorInfo();
                break;
            case VectorType.Addition_operand1:
                startPt = operand1.vectorInfo;
                endPt = operand1.vectorInfo + operand2.vectorInfo;
                break;
            case VectorType.Addition_operand2:
                startPt = operand2.vectorInfo;
                endPt = operand1.vectorInfo + operand2.vectorInfo;
                break;
            case VectorType.CrossProduct:
                endPt = Vector3.Cross(operand1.vectorInfo, operand2.vectorInfo);
                ShowVectorInfo();
                break;
            case VectorType.VectorField:
                break;
            case VectorType.UserCreated:
                if (lerpStartTime.HasValue == true)
                    endPt = Vector3.Lerp(prevPos, targetPos, (Time.time - lerpStartTime.Value) / 2.0f);

                if (selected)
                {
                    lr = GetComponent<LineRenderer>();
                    lr.startColor = new Color(217f / 255f, 100f / 255f, 142f / 255f);
                    lr.endColor = new Color(217f / 255f, 100f / 255f, 142f / 255f);
                }
                else
                {
                    //GetComponent<LineRenderer>().SetColors(Color.white, Color.blue);
                    lr = GetComponent<LineRenderer>();
                    lr.startColor = Color.white;
                    lr.endColor = Color.white;
                }
                ShowVectorInfo();
                break;
        }
        GetComponent<LineRenderer>().SetPosition(0, startPt);
        GetComponent<LineRenderer>().SetPosition(1, endPt);

        UpdateArrowTip(arrowTip);
    }

    /************************
    * Enabling lerp animation
    ************************/
    public void SetLerping(Vector3 start, Vector3 end)
    {
        startPt = start;
        targetPos = end;
        prevPos = endPt;
        lerpStartTime = Time.time;
    }

    /********************************
    * Set vector position w/o lerping
    ********************************/
    public void SetPosition(Vector3 start, Vector3 end)
    {
        startPt = start;
        prevPos = endPt = targetPos = end;
    }

    public void SetVectorType(VectorType type0, MathBox_Vector op1, MathBox_Vector op2)
    {
        type = type0;
        operand1 = op1;
        operand2 = op2;
    }

    private GameObject InitArrowTip()
    {
        Vector3 forward = vectorInfo.normalized;
        Vector3 tipPos = endPt - forward * 0.1f;
        Quaternion tipRot = Quaternion.LookRotation(startPt - endPt);
        GameObject tip = (GameObject)Instantiate(tipPrefab, tipPos, tipRot);
        tip.transform.SetParent(transform, false);
        return tip;
    }

    private void UpdateArrowTip(GameObject tip)
    {
        Vector3 forward = vectorInfo.normalized;
        Vector3 tipPos = endPt - forward * 0.1f;
        Quaternion tipRot = Quaternion.LookRotation(startPt - endPt);
        tip.transform.position = tipPos;
        tip.transform.rotation = tipRot;
        //tip.transform.SetParent(transform, false);
    }

    public void CreateInCartesian()
    {
        targetPos = new Vector3(Mathf.Round(endPt.x), Mathf.Round(endPt.y), Mathf.Round(endPt.z));
        prevPos = startPt;
        lerpStartTime = Time.time;
    }

    public void CreateInGeneral()
    {
        targetPos = endPt;
        prevPos = startPt;
        lerpStartTime = Time.time;
    }

    public void FinishEditInCartesian()
    {
        endPt = targetPos = prevPos = new Vector3(Mathf.Round(endPt.x), Mathf.Round(endPt.y), Mathf.Round(endPt.z));
        lerpStartTime = null;
    }

    public void FinishEditInGeneral()
    {
        targetPos = prevPos = endPt;
        lerpStartTime = null;
    }

    public void select()
    {
        selected = !selected;
    }

    /********************************************
    * Converting between cartesian, spherical and 
    * cylindrical coordinate systems
    ********************************************/
    void CarSphCyl()
    {
        x = vectorInfo.x;
        y = vectorInfo.z;
        z = vectorInfo.y;

        r = Mathf.Sqrt(Mathf.Pow(x, 2) + Mathf.Pow(y, 2));

        theta = Mathf.Atan2(y, x);
        
        
        if(y < 0)
        {
            theta += Mathf.PI * 2.0f;
        }

        rho = Mathf.Sqrt(Mathf.Pow(r, 2) + Mathf.Pow(z, 2));
        phi = Mathf.Atan2(r, z);

        display_phi = RadInPi(phi);
        display_theta = RadInPi(theta);
    }

    void ShowVectorInfo()
    {
        CarSphCyl();

        switch (scene.coordType)
        {
            case InteractionHandler.CoordSystemType.CARTESIAN:
                GetComponentInChildren<TextMesh>().text = "|" + x + "|\n|" + y + "|\n|" + z + "|";
                GetComponentInChildren<TextMesh>().transform.position = endPt;
                break;
            case InteractionHandler.CoordSystemType.SPHERICAL:
                //GetComponentInChildren<TextMesh>().text = "|" + rho + "|\n|" + phi + "|\n|" + theta + "|";
                GetComponentInChildren<TextMesh>().text = "|" + rho + "|\n|" + display_phi + "|\n|" + display_theta + "|";
                GetComponentInChildren<TextMesh>().transform.position = endPt;
                break;
            case InteractionHandler.CoordSystemType.CYLINDRICAL:
                //GetComponentInChildren<TextMesh>().text = "|" + r + "|\n|" + theta + "|\n|" + z + "|";
                GetComponentInChildren<TextMesh>().text = "|" + r + "|\n|" + display_theta + "|\n|" + z + "|";
                GetComponentInChildren<TextMesh>().transform.position = endPt;
                break;
        }
    }

    string RadInPi(float radian)
    {
        //string display = "";
        int x = Mathf.RoundToInt(radian * 36.0f / Mathf.PI);
        if (x % 72 == 0) // 2pi
        {
            return "0";
        }
        else if (x % 36 == 0) // pi
        {
            return "π";
        }
        else if (x % 18 == 0) // x/2 pi
        {
            return (x / 18).ToString() + "π/2";
        }
        else if (x % 12 == 0) // x/3 pi
        {
            return (x / 12).ToString() + "π/3";
        }
        else if (x % 9 == 0)
        {
            return (x / 9).ToString() + "π/4";
        }
        else if (x % 6 == 0)
        {
            return (x / 6).ToString() + "π/6";
        }
        else if (x % 4 == 0)
        {
            return (x / 4).ToString() + "π/9";
        }
        else if (x % 3 == 0)
        {
            return (x / 3).ToString() + "π/12";
        }
        else if (x % 2 == 0)
        {
            return (x / 2).ToString() + "π/18";
        }
        else
        {
            return x.ToString() + "π/36";
        }
    }

    public void ClearVector()
    {
        Destroy(arrowTip);
        Destroy(gameObject);
    }
}
