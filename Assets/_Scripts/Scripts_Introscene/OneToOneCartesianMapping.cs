using UnityEngine;
using System.Collections;

[ExecuteInEditMode]
public class OneToOneCartesianMapping : MonoBehaviour {

    public Transform point;

	// Use this for initialization
	void Start () {
	
	}
	
	// Update is called once per frame
	void Update () {
        transform.localPosition = MapFunction(point.localPosition);
	}

    Vector3 MapFunction(Vector3 v)
    {
        float theta = (v.x + 10f) / 10f * Mathf.PI; //[-10,10]->[0,2pi]
        float phi = (v.z + 10f) / 20f * Mathf.PI; //[-10,10]->[0,pi]
        float rho = (v.y + 10f) / 20f; //[-10,10]->[0,1]

        float x = rho * Mathf.Sin(phi) * Mathf.Cos(theta);
        float y = rho * Mathf.Sin(phi) * Mathf.Sin(theta);
        float z = rho * Mathf.Cos(phi);

        x *= 10f; //[-1,1]->[-10,10]
        y *= 10f; //[-1,1]->[-10,10]
        z *= 10f; //[-1,1]->[-10,10]

        return new Vector3(x, z, y);
    }
}
