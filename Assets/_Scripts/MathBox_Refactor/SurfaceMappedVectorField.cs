using UnityEngine;
using System.Collections;
using System.Collections.Generic;
using MathFunctions;

public class SurfaceMappedVectorField : MonoBehaviour {

    List<Transform> vectors;
    List<Vector3> startPts;
    List<Vector3> offsets;
    float max_magnitude;

    ParticleSystem shuriken;
    ParticleSystem.Particle[] particles;

    public TwoVariableFunctions.TwoVarFnOptions sampleFunction;
    TwoVariableFunctions.TwoVarFnOptions currFunction;

    public VectorFieldFunctions.VecFieldFnOptions vectorField;
    VectorFieldFunctions.VecFieldFnOptions currVecField;

    public Transform vPrefab;

    public float min_x = -3;
    public float max_x = 3;
    public float min_z = -3;
    public float max_z = 3;

	// Use this for initialization
	void Start () {
        //shuriken = GetComponent<ParticleSystem>();
        //particles = new ParticleSystem.Particle[shuriken.maxParticles];

        vectors = new List<Transform>();
        startPts = new List<Vector3>();
        offsets = new List<Vector3>();

        MapVectorField(VectorFieldFunctions.vecFieldFns[(int)vectorField]);
        DrawVectorField();
	}
	
	// Update is called once per frame
	void Update () {
        //AnimateParticleFlow();
	}

    void MapVectorField(VectorFieldFunctions.VectorFieldFunction myFn)
    {
        max_magnitude = 0f;

        for(float x= min_x; x <= max_x; x++)
        {
            for(float z= min_z; z < max_z; z++)
            {
                Vector3 start = new Vector3(x, 0, z);
                start = new Vector3(x, TwoVariableFunctions.twoVarFns[(int)sampleFunction](x, z), z);
                startPts.Add(start);
                Vector3 offset = myFn(start);
                if(offset.magnitude > max_magnitude)
                {
                    max_magnitude = offset.magnitude;
                }
                offsets.Add(offset);
            }
        }
    }

    public void DrawVectorField()
    {
        for (int i = 0; i < startPts.Count; i++)
        {
            Vector3 target = startPts[i];
            Vector3 offset = offsets[i] / max_magnitude;
            Vector3 tip = offset * 0.4f;

            Transform l = Instantiate(vPrefab);
            l.SetParent(transform, false);
            LineRenderer top = l.Find("Top").GetComponent<LineRenderer>();
            top.SetPosition(0, target + offset - tip);
            top.SetPosition(1, target + offset);
            LineRenderer body = l.Find("Body").GetComponent<LineRenderer>();
            body.SetPosition(0, target + offset - tip);
            body.SetPosition(1, target);
            vectors.Add(l);

            //Color c = gradient.Evaluate(offset.magnitude);
            //top.SetColors(c, c);
            //body.SetColors(c, c);
        }
    }

    void AnimateParticleFlow()
    {
        int count = shuriken.GetParticles(particles);

        for (int i = 0; i < count; i++)
        {
            Vector3 velocity = VectorFieldFunctions.vecFieldFns[(int)vectorField](particles[i].position);
            velocity.y = 0;
            particles[i].velocity = velocity;
            //particles[i].startColor = gradient.Evaluate(velocity.magnitude);
        }

        shuriken.SetParticles(particles, particles.Length);
    }
}
