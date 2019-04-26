using UnityEngine;
using System.Collections;

public class Arrow : MonoBehaviour {
    public enum ResultType { Addition, Addition_helper1, Addition_helper2, CrossProduct };

    private Vector4 startpos;
    private Vector4 endpos;
    bool selected = false;

    bool result_vec = false;
    ResultType resultType;
    Arrow operand1 = null;
    Arrow operand2 = null;

	GameObject camPoint;
    private Vector4 vectorInfo
    {
        get
        {
            return startpos - endpos;
        }
    }

    private Vector4 origin_pos
    {
        get
        {
            return GameObject.FindGameObjectWithTag("Origin").transform.position;
        }
    }
    
    private GameObject thisarrow;
    public GameObject arrowpoint;
    private float starttime = -5;
    private Vector4 newend;
    private TextMesh text;
    private Vector4 prevpos;

    //CGHW
    float x, y, z, rho, theta, phi, crho, ctheta;
    Vector3 vec1, vec2;
    private TextMesh coordtext;
    public static int Cartesian = 0;
    public static int Spherical = 1;
    public static int Cylindrical = 2;

    public Arrow(float x, float y, float z)
    {
        startpos = Vector4.zero;
        endpos = new Vector4(x, y, z, 1);
    }

    public Arrow()
    {
        //startpos = Vector3.zero;
        //endpos = Vector3.zero;
    }

    public Arrow(Vector3 end)
    {
        startpos = Vector4.zero;
        endpos = end;
    }

    public Arrow(Vector4 start, Vector4 end)
    {
        startpos = start;
        endpos = end;
    }

    public void lerpPoints(Vector4 start, Vector4 end)
    {
        startpos = start;
        newend = end;
        prevpos = endpos;
        starttime = Time.time;
    }

    public void setPoints(Vector4 start, Vector4 end)
    {
        startpos = start;
        endpos = end;
        newend = end;
        prevpos = end;
        //starttime = Time.time;
    }

	public void setCamRotation(GameObject cam)
	{
		camPoint = cam;
	}

    public void setResultVec(bool result, ResultType type, Arrow op1, Arrow op2)
    {
        result_vec = result;
        resultType = type;
        operand1 = op1;
        operand2 = op2;
    }

	// Use this for initialization
	void Start () {
        //setPosition();
        GetComponent<LineRenderer>().SetPosition(0, startpos);
        GetComponent<LineRenderer>().SetPosition(1, startpos + (endpos - startpos));
        //GetComponent<LineRenderer>().SetPosition(1, endpos);
        Vector4 unit = (endpos - startpos).normalized;
        Vector4 arrowmid = new Vector4((endpos.x - (unit.x * .1f)), (endpos.y - (unit.y * .1f)), (endpos.z - (unit.z * .1f)), 0);
        Quaternion arrowRotation = Quaternion.LookRotation(startpos - endpos);
        thisarrow = (GameObject)Instantiate(arrowpoint, arrowmid, arrowRotation);
        thisarrow.transform.parent = this.transform;
        //thisarrow.transform.position = arrowmid;
        //thisarrow.transform.rotation = arrowRotation;
        if (MakinVectors.toggle == Cartesian)
        {
            // Display "Cartesian"
            gameObject.GetComponentInChildren<TextMesh>().text = "|" + endpos.x + "|\n|" + endpos.y + "|\n|" + endpos.z + "|";
            gameObject.GetComponentInChildren<TextMesh>().transform.position = new Vector4(endpos.x, endpos.y, endpos.z, 1);
            gameObject.GetComponentInChildren<TextMesh>().transform.rotation = Quaternion.LookRotation(camPoint.transform.position - new Vector3(endpos.x, endpos.y, endpos.z)) * Quaternion.Euler(0, 180, 0);
            gameObject.GetComponentInChildren<TextMesh>().characterSize = .02f;
        }

        else if (MakinVectors.toggle == Spherical)
        {
            // Display "Spherical"
            vec1 = endpos;
            CartesianToSpherical(vec1, out rho, out theta, out phi);
            string dstheta = RadToPi(theta);
            string dsphi = RadToPi(phi);
            gameObject.GetComponentInChildren<TextMesh>().text = "|" + rho + "|\n|" + dstheta + "|\n|" + dsphi + "|";
            gameObject.GetComponentInChildren<TextMesh>().transform.position = new Vector4(endpos.x, endpos.y, endpos.z, 1);
            gameObject.GetComponentInChildren<TextMesh>().transform.rotation = Quaternion.LookRotation(camPoint.transform.position - new Vector3(endpos.x, endpos.y, endpos.z)) * Quaternion.Euler(0, 180, 0);
            gameObject.GetComponentInChildren<TextMesh>().characterSize = .02f;
        }

        else if (MakinVectors.toggle == Cylindrical)
        {
            // Display "Cylindrical"
            vec2 = endpos;
            //SphericalToCartesian(rho, theta, phi, out vec1);
            CartesianToCylindrical(vec2, out crho, out ctheta);
            string dctheta = RadToPi(ctheta);
            gameObject.GetComponentInChildren<TextMesh>().text = "|" + crho + "|\n|" + dctheta + "|\n|" + endpos.y + "|";
            gameObject.GetComponentInChildren<TextMesh>().transform.position = new Vector4(endpos.x, endpos.y, endpos.z);
            gameObject.GetComponentInChildren<TextMesh>().transform.rotation = Quaternion.LookRotation(camPoint.transform.position - new Vector3(endpos.x, endpos.y, endpos.z)) * Quaternion.Euler(0, 180, 0);
            gameObject.GetComponentInChildren<TextMesh>().characterSize = .02f;
        }
    }

    // Update is called once per frame
    void Update () {
        if (result_vec)
        {
            switch (resultType)
            {
                case ResultType.Addition:
                    newend = operand1.getInfo() + operand2.getInfo();
                    endpos = Vector4.Lerp(prevpos, newend, (Time.time - starttime) / 2.0f);
                    setPosition();
                    break;
                case ResultType.Addition_helper1:
                    startpos = operand1.getInfo();
                    endpos = operand1.getInfo() + operand2.getInfo();
                    setPosition();
                    break;
                case ResultType.Addition_helper2:
                    startpos = operand2.getInfo();
                    endpos = operand1.getInfo() + operand2.getInfo();
                    setPosition();
                    break;
                case ResultType.CrossProduct:
                    newend = Vector3.Cross(operand1.getInfo(), operand2.getInfo());
                    endpos = Vector4.Lerp(prevpos, newend, (Time.time - starttime) / 2.0f);
                    //endpos = newend;
                    setPosition();
                    break;
            }
            return;
        }

        //Destroy(thisarrow);
        endpos = Vector4.Lerp(prevpos, newend, (Time.time - starttime) / 2.0f);
        setPosition();
        if (selected)
        {
            gameObject.GetComponent<LineRenderer>().startColor = Color.white;
            gameObject.GetComponent<LineRenderer>().endColor = new Color(1, 0, 0, 1);
        }
        else
        {
            gameObject.GetComponent<LineRenderer>().startColor = Color.white;
            gameObject.GetComponent<LineRenderer>().endColor = new Color(0, 0, 1, 1);
        }        
    }

    public Vector4 getInfo()
    {
        return endpos;
    }

    public Vector4 getStart()
    {

        //return startpos;
        return Vector4.zero;
    }

    public void add(Vector4 other)
    {
        newend = endpos + other;
        prevpos = endpos;
        starttime = Time.time;
    }

    public static Arrow operator +(Arrow first, Arrow second)
    {
        return new Arrow(first.getInfo() + second.getInfo());
    }

    public static Arrow operator +(Arrow first, Vector4 second)
    {
        return new Arrow(first.getInfo() + second);
    }

    void setPosition()
    {
        Vector4 origin = origin_pos;
        GetComponent<LineRenderer>().SetPosition(0, startpos);
        GetComponent<LineRenderer>().SetPosition(1, startpos + (endpos - startpos));
        //GetComponent<LineRenderer>().SetPosition(1, endpos);
        Vector4 unit = (endpos - startpos).normalized;
        Vector4 arrowmid = new Vector4((endpos.x - (unit.x * .1f)), (endpos.y - (unit.y * .1f)), (endpos.z - (unit.z * .1f) ), 0 );
		Quaternion arrowRotation = Quaternion.LookRotation(startpos - endpos);
        //thisarrow = (GameObject)Instantiate(arrowpoint, arrowmid, arrowRotation);
        //thisarrow.transform.parent = this.transform;
        thisarrow.transform.position = arrowmid;
        thisarrow.transform.rotation = arrowRotation;
        if (MakinVectors.toggle == Cartesian)
        {
            // Display "Cartesian"
            gameObject.GetComponentInChildren<TextMesh>().text = "|" + endpos.x + "|\n|" + endpos.y + "|\n|" + endpos.z + "|";
            gameObject.GetComponentInChildren<TextMesh>().transform.position = new Vector4(endpos.x, endpos.y, endpos.z, 1);
            gameObject.GetComponentInChildren<TextMesh>().transform.rotation = Quaternion.LookRotation(camPoint.transform.position - new Vector3(endpos.x, endpos.y, endpos.z)) * Quaternion.Euler(0, 180, 0);
            gameObject.GetComponentInChildren<TextMesh>().characterSize = .02f;
        }

        else if (MakinVectors.toggle == Spherical)
        {
            // Display "Spherical"
            vec1 = endpos;
            CartesianToSpherical(vec1, out rho, out theta, out phi);
            string dstheta = RadToPi2(theta);
            string dsphi = RadToPi2(phi);
            gameObject.GetComponentInChildren<TextMesh>().text = "|" + rho + "|\n|" + dstheta + "|\n|" + dsphi + "|";
            gameObject.GetComponentInChildren<TextMesh>().transform.position = new Vector4(endpos.x, endpos.y, endpos.z, 1);
            gameObject.GetComponentInChildren<TextMesh>().transform.rotation = Quaternion.LookRotation(camPoint.transform.position - new Vector3(endpos.x, endpos.y, endpos.z)) * Quaternion.Euler(0, 180, 0);
            gameObject.GetComponentInChildren<TextMesh>().characterSize = .02f;
        }

        else if (MakinVectors.toggle == Cylindrical)
        {
            // Display "Cylindrical"
            vec2 = endpos;
            //SphericalToCartesian(rho, theta, phi, out vec1);
            CartesianToCylindrical(vec2, out crho, out ctheta);
            string dctheta = RadToPi(ctheta);
            gameObject.GetComponentInChildren<TextMesh>().text = "|" + crho + "|\n|" + dctheta + "|\n|" + endpos.y + "|";
            gameObject.GetComponentInChildren<TextMesh>().transform.position = new Vector4(endpos.x, endpos.y, endpos.z);
            gameObject.GetComponentInChildren<TextMesh>().transform.rotation = Quaternion.LookRotation(camPoint.transform.position - new Vector3(endpos.x, endpos.y, endpos.z)) * Quaternion.Euler(0, 180, 0);
            gameObject.GetComponentInChildren<TextMesh>().characterSize = .02f;
        }

    }

    public void CartesianToSpherical(Vector3 cartCoords, out float outRadius, out float outPolar, out float outElevation)
    {
        if (cartCoords.x == 0)
        {
            cartCoords.x = Mathf.Epsilon;
        }

        outRadius = Mathf.Sqrt((cartCoords.x * cartCoords.x)
            + (cartCoords.y * cartCoords.y)
            + (cartCoords.z * cartCoords.z));

        outPolar = Mathf.Atan(cartCoords.z / cartCoords.x);

        if (cartCoords.x < 0 && cartCoords.z >= 0)
        {
            outPolar += Mathf.PI;
        }
        if (cartCoords.z < 0)
        {
            outPolar += Mathf.PI;
        }
        if (cartCoords.z < 0 && cartCoords.x >= 0)
        {
            outPolar += Mathf.PI;
        }


        outElevation = Mathf.Asin(cartCoords.y / outRadius);
    } // End of CartesianToSpherical()

    public void SphericalToCartesian(float radius, float polar, float elevation, out Vector3 outCart)
    {
        float a = radius * Mathf.Cos(elevation);
        outCart.x = a * Mathf.Cos(polar);
        outCart.y = radius * Mathf.Sin(elevation);
        outCart.z = a * Mathf.Sin(polar);
    } // End of SphericalToCartesian()

    public void CartesianToCylindrical(Vector3 cartCoords, out float outRadius, out float outPolar)
    {
        outRadius = Mathf.Sqrt(cartCoords.x * cartCoords.x + cartCoords.z * cartCoords.z);
        outPolar = 0.0f;
        if (cartCoords.x == 0 && cartCoords.z == 0)
        {
            outPolar = 0;
        }
        outPolar = Mathf.Atan(cartCoords.z / cartCoords.x);

        if (cartCoords.x < 0 && cartCoords.z >= 0)
        {
            outPolar += Mathf.PI;
        }
        if (cartCoords.z < 0)
        {
            outPolar += Mathf.PI;
        }
        if (cartCoords.z < 0 && cartCoords.x >= 0)
        {
            outPolar += Mathf.PI;
        }


    } // End of CartesianToCylindrical()

    public string RadToPi(float angle)
    {
        string display;
        int x, top;
        x = (int)Mathf.Round(angle * 8 / Mathf.PI);
        display = "";
        if (x % 16 == 0)
        {
            display = "0";
            return display;
        }
        else if (x % 8 == 0)
        {
            display = "π";
            return display;
        }
        else if (x % 4 == 0)
        {
            top = x / 4;
            if (top == -1)
                display = "-π/2";
            else if (top == 1)
                display = "π/2";
            else
                display = top.ToString() + "π/2";
            return display;
        }
        else if (x % 2 == 0)
        {
            top = x / 2;
            if (top == -1)
                display = "-π/4";
            else if (top == 1)
                display = "π/4";
            else
                display = top.ToString() + "π/4";
            return display;
        }
        else {
            if (x == -1)
                display = "-π/8";
            else if (x == 1)
                display = "π/8";
            else
                display = x.ToString() + "π/8";
            return display;
        }
    }

    public string RadToPi2(float radian)
    {
        string display = "";
        int x = Mathf.RoundToInt(radian * 36.0f / Mathf.PI);
        if(x % 72 == 0) // 2pi
        {
            display = "0";
        }
        else if(x % 36 == 0) // pi
        {
            display = "π";
        }
        else if(x % 18 == 0)
        {
            display = (x / 18).ToString() + "π/2";
        }
        else if (x % 12 == 0)
        {
            display = (x / 12).ToString() + "π/3";
        }
        else if (x % 9 == 0)
        {
            display = (x / 9).ToString() + "π/4";
        }
        else if ( x % 6 == 0)
        {
            display = (x / 6).ToString() + "π/6";
        }
        else if(x % 4 == 0)
        {
            display = (x / 4).ToString() + "π/9";
        }
        else if (x % 3 == 0)
        {
            display = (x / 3).ToString() + "π/12";
        }
        else if (x%2 == 0)
        {
            display = (x / 2).ToString() + "π/18";
        }
        else
        {
            display = x.ToString() + "π/36";
        }
        return display;
    }

    public void SetText()
    {

    }

    public void finalize()
    {
        newend.x = Mathf.Round(endpos.x);
        newend.y = Mathf.Round(endpos.y);
        newend.z = Mathf.Round(endpos.z);
        prevpos = startpos;
        starttime = Time.time;
    }

    public void FinishEditing()
    {
        newend.x = Mathf.Round(endpos.x);
        newend.y = Mathf.Round(endpos.y);
        newend.z = Mathf.Round(endpos.z);
        prevpos = newend;
        //prevpos = startpos;
        starttime = Time.time;
    }

    public void select()
    {
        if(selected)
        {
            selected = false;
        }
        else
        {
            selected = true;
        }
    }

    public void removeShip()
    {
		Destroy (thisarrow);
    }
}
