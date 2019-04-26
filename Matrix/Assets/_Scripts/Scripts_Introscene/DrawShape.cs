using UnityEngine;
using System.Collections;

public class DrawShape : MonoBehaviour {

    public ConstraintGrabbable pivotSphere;

	SteamVR_TrackedObject trackedObj;
	//private bool is_touching_sphere = false;
	GameObject sphere;
	//private bool grabbed = false;

	public PlotterPartial frontFacePrefab;
	public PlotterPartial backFacePrefab;
	public PlotterPartial leftFacePrefab;
	public PlotterPartial rightFacePrefab;
    public PlotterPartial bottomFacePrefab;


	private PlotterPartial frontFace;
	private PlotterPartial backFace;
	private PlotterPartial leftFace;
	private PlotterPartial rightFace;
    private PlotterPartial bottomFace;

	public float minX, maxX, minZ, maxZ, startX, startZ;
	private float orig_minX, orig_maxX, orig_minZ, orig_maxZ;
	private float offset = 0.2f;

    float myFn(float x, float z)
    {
        //return Mathf.Pow (Mathf.Cos (x), 2f) + Mathf.Pow (Mathf.Cos (z), 2f) + 3.0f;
        return .5f * Mathf.Cos(x) + .5f * Mathf.Cos(z) + 1;
    }

    // Use this for initialization
    void Start () {
		orig_minX = minX;
		orig_maxX = maxX;
		orig_minZ = minZ;
		orig_maxZ = maxZ;

		frontFace = (PlotterPartial) Instantiate (frontFacePrefab, Vector3.zero, Quaternion.identity);
        frontFace.transform.SetParent(transform.parent, false);
		frontFace.SampleCurve (myFn, startX, null, minZ, startZ, offset);
		frontFace.DrawMesh (0.0f);

		backFace = (PlotterPartial) Instantiate (backFacePrefab, Vector3.zero, Quaternion.identity);
        backFace.transform.SetParent(transform.parent, false);
        backFace.SampleCurve (myFn, minX, null, minZ, startZ, offset);
		backFace.DrawMesh (0.0f);

		leftFace = (PlotterPartial) Instantiate (leftFacePrefab, Vector3.zero, Quaternion.identity);
        leftFace.transform.SetParent(transform.parent, false);
        leftFace.SampleCurve (myFn, minX, startX, minZ, null, offset);
		leftFace.DrawMesh (0.0f);

		rightFace = (PlotterPartial) Instantiate (rightFacePrefab, Vector3.zero, Quaternion.identity);
        rightFace.transform.SetParent(transform.parent, false);
        rightFace.SampleCurve (myFn, minX, startX, startZ, null, offset);
		rightFace.DrawMesh (0.0f);

		//sphere = GameObject.CreatePrimitive (PrimitiveType.Sphere);
		//sphere.transform.position = new Vector3 (startX, -3.0f, startZ);
		//sphere.transform.localScale = new Vector3 (0.5f, 0.5f, 0.5f);
	}

	void FixedUpdate () {
        DrawVolume(pivotSphere.lastLocalPos.x, pivotSphere.lastLocalPos.y, pivotSphere.lastLocalPos.z);
	}

	public void DrawVolume (float x, float y, float z) {
		// Round to nearest .2 to match offset
		minX = (float) System.Math.Round (minX * 5, System.MidpointRounding.ToEven) / 5.0f;
		float tempMaxX = (float) System.Math.Round (x * 5, System.MidpointRounding.ToEven) / 5.0f;

		if (minX <= orig_maxX && tempMaxX >= orig_minX && minX >= orig_minX && tempMaxX <= orig_maxX && minX <= tempMaxX) {
			maxX = tempMaxX;

			frontFace.ClearData ();
			backFace.ClearData ();
			leftFace.ClearData ();
			rightFace.ClearData ();

			frontFace.SampleCurve (myFn, maxX, null, minZ, maxZ, offset);
			frontFace.DrawMesh (y);

			backFace.SampleCurve (myFn, minX, null, minZ, maxZ, offset);
			backFace.DrawMesh (y);

			leftFace.SampleCurve (myFn, minX, maxX, minZ, null, offset);
			leftFace.DrawMesh (y);

			rightFace.SampleCurve (myFn, minX, maxX, maxZ, null, offset);
			rightFace.DrawMesh (y);
		}



		minZ = (float) System.Math.Round (minZ * 5, System.MidpointRounding.ToEven) / 5.0f;
		float tempMaxZ = (float) System.Math.Round (z * 5, System.MidpointRounding.ToEven) / 5.0f;

		if (minZ <= orig_maxZ && tempMaxZ >= orig_minZ && minZ >= orig_minZ && tempMaxZ <= orig_maxZ && minZ <= tempMaxZ) {
			maxZ = tempMaxZ;

			frontFace.ClearData ();
			backFace.ClearData ();
			leftFace.ClearData ();
			rightFace.ClearData ();

			frontFace.SampleCurve (myFn, maxX, null, minZ, maxZ, offset);
			frontFace.DrawMesh (y);

			backFace.SampleCurve (myFn, minX, null, minZ, maxZ, offset);
			backFace.DrawMesh (y);

			leftFace.SampleCurve (myFn, minX, maxX, minZ, null, offset);
			leftFace.DrawMesh (y);

			rightFace.SampleCurve (myFn, minX, maxX, maxZ, null, offset);
			rightFace.DrawMesh (y);
		}
	}
}
