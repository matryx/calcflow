using UnityEngine;
using System.Collections;

public class SphericalCoordinateMapping : MonoBehaviour {

    public ConstraintGrabbable twoDPoint;

    public Transform corespondingPoint;
    LineRenderer dtheta_top, dtheta_body,
        dphi_top, dphi_body,
        normal_top, normal_body;

	// Use this for initialization
	void Start () {
        dtheta_top = corespondingPoint.Find("dTheta").Find("Top").GetComponent<LineRenderer>();
        dtheta_body = corespondingPoint.Find("dTheta").Find("Body").GetComponent<LineRenderer>();

        dphi_top = corespondingPoint.Find("dPhi").Find("Top").GetComponent<LineRenderer>();
        dphi_body = corespondingPoint.Find("dPhi").Find("Body").GetComponent<LineRenderer>();

        normal_top = corespondingPoint.Find("Normal").Find("Top").GetComponent<LineRenderer>();
        normal_body = corespondingPoint.Find("Normal").Find("Body").GetComponent<LineRenderer>();
    }

    // Update is called once per frame
    void Update () {
        corespondingPoint.localPosition = TwoDMap(twoDPoint.lastLocalPos);
        Vector3 dTheta = TwoDMap(twoDPoint.lastLocalPos + new Vector3(0, 0, 0.0001f));
        dTheta -= corespondingPoint.localPosition;
        dTheta = dTheta.normalized;
        Vector3 dPhi = TwoDMap(twoDPoint.lastLocalPos + new Vector3(0, 0.0001f, 0));
        dPhi -= corespondingPoint.localPosition;
        dPhi = dPhi.normalized;
        //Vector3 cross = Vector3.Cross(dPhi, dTheta) * ((twoDPoint.localPosition.y > 0) ? -1 : 1);
        Vector3 cross = Vector3.Cross(dPhi, dTheta);
        cross = cross.normalized;

        dtheta_body.SetPosition(0, Vector3.zero);
        dtheta_body.SetPosition(1, dTheta * 1.5f);
        dtheta_top.SetPosition(0, dTheta * 1.5f);
        dtheta_top.SetPosition(1, dTheta * 3f);

        dphi_body.SetPosition(0, Vector3.zero);
        dphi_body.SetPosition(1, dPhi * 1.5f);
        dphi_top.SetPosition(0, dPhi * 1.5f);
        dphi_top.SetPosition(1, dPhi * 3f);

        normal_body.SetPosition(0, Vector3.zero);
        normal_body.SetPosition(1, cross * 1.5f);
        normal_top.SetPosition(0, cross * 1.5f);
        normal_top.SetPosition(1, cross * 3f);
    }

    Vector3 TwoDMap(Vector3 position)
    {
        float theta = position.z;
        theta = theta / 10f * Mathf.PI;
        float phi = position.y;
        phi = -phi / 10f * Mathf.PI / 2f + Mathf.PI / 2f;

        float radius = transform.localScale.x / 2f;

        float x = radius * Mathf.Sin(phi) * Mathf.Cos(theta);
        float y = radius * Mathf.Sin(phi) * Mathf.Sin(theta);
        float z = radius * Mathf.Cos(phi);

        return new Vector3(x, z, y);
    }
}
