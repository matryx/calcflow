using UnityEngine;
using System.Collections;
using MathFunctions;

public class ParametrizedSurface : MonoBehaviour {

    public float uMin, uMax;
    public float vMin, vMax;

    ParticleSystem shuriken;
    ParticleSystem.Particle[] points;

    public int resolution;
    //private int currResolution;

    public Gradient gradient;

    public ParametrizedEquations.ParametrizationOptions sampleFunction;
    private ParametrizedEquations.ParametrizationOptions currFn;

	// Use this for initialization
	void Start () {
        shuriken = GetComponent<ParticleSystem>();

        points = new ParticleSystem.Particle[resolution * resolution];

        GenerateParticles();
        shuriken.SetParticles(points, points.Length);
	}
	
	// Update is called once per frame
	void Update () {
        //shuriken.SetParticles(points, points.Length);
    }

    void GenerateParticles()
    {
        //currResolution = resolution;

        float u_size = uMax - uMin;
        float v_size = vMax - vMin;

        points = new ParticleSystem.Particle[resolution * resolution];

        float uoffset = (float)u_size / (float)(resolution - 1);
        float voffset = (float)v_size / (float)(resolution - 1);

        ParametrizedEquations.ParametrizedEquation fn =
            ParametrizedEquations.paramFunctions[(int)sampleFunction];

        int i = 0;
        for(int u = 0; u < resolution; u++)
        {
            for(int v = 0; v < resolution; v++)
            {
                Vector3 pos = fn((uMin + u * uoffset) * Mathf.PI, ((vMin + v * voffset) * Mathf.PI));
                points[i].position = pos;
                points[i].startColor = gradient.Evaluate((v * voffset) / (vMax - vMin));
                //points[i++].startSize = Mathf.Sqrt(uoffset * voffset);
                points[i++].startSize = 0.1f;
            }
        }
    }
}
