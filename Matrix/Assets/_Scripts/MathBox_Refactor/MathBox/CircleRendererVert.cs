using UnityEngine;
using System.Collections;

public class CircleRendererVert : MonoBehaviour
{
    public float degree_per_seg;
    public float radialScale;

    private float curAngle;
    private LineRenderer line;
    private int vertexCount;

    // Use this for initialization
    void Awake()
    {
        line = GetComponent<LineRenderer>();
        vertexCount = (int)(360 / degree_per_seg + 1);
        line.positionCount = vertexCount;
    }

    void Start()
    {
        GeneratePts();
    }

    void GeneratePts()
    {
        float x, y, z;
        z = 0;
        for (int i = 0; i < vertexCount; i++)
        {
            x = Mathf.Sin(Mathf.Deg2Rad * curAngle);
            y = Mathf.Cos(Mathf.Deg2Rad * curAngle);
            line.SetPosition(i, new Vector3(x, y, z) * radialScale);
            curAngle += degree_per_seg;
        }

    }
}
