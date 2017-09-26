/*******************************
* Visualization of vector fields
*******************************/

using UnityEngine;
using System;
using System.Collections;
using System.Collections.Generic;
using MathFunctions;

public class VectorField : MonoBehaviour {

    public Gradient gradient;

    //public MathBox_Vector vectorPrefab;
    public Transform vPrefab;

    //List<MathBox_Vector> vectors;
    List<Transform> vectors;
    List<Vector3> startPts;
    List<Vector3> offsets;
    float max_magnitude;

    ParticleSystem.Particle[] particles;
    ParticleSystem shuriken;
    List<ParticleSystem.Particle> points;

    public VectorFieldFunctions.VecFieldFnOptions sampleFunction;
    VectorFieldFunctions.VecFieldFnOptions currFn;

    /*
    * Samples a vector function on a certain domain
    * and stores the results
    */
    public void SampleVectorField(VectorFieldFunctions.VectorFieldFunction myFn, float x_min, float x_max, float y_min, float y_max, float z_min, float z_max, float delta)
    {
        currFn = sampleFunction;

        max_magnitude = 0f;

        for(float x_temp = x_min; x_temp <= x_max; x_temp+=delta)
        {
            for(float y_temp = y_min; y_temp <= y_max; y_temp+=delta)
            {
                for (float z_temp = z_min; z_temp <= z_max; z_temp += delta)
                {
                    Vector3 target = new Vector3(x_temp, y_temp, z_temp);
                    Vector3 result = myFn(target);
                    Vector3 direction = result.normalized;
                    float length = result.magnitude;
                    if(length > max_magnitude)
                    {
                        max_magnitude = length;
                    }
                    startPts.Add(target);
                    offsets.Add(direction * length);
                }
            }
        }
    }

    void SetGradient()
    {
        GradientColorKey[] gck = new GradientColorKey[3];
        gck[0].color = Color.blue;gck[0].time = 0f;
        gck[1].color = Color.green;gck[1].time = 0.5f;
        gck[2].color = Color.red;gck[2].time = 1f;
        GradientAlphaKey[] gak = new GradientAlphaKey[2];
        gak[0].alpha = 1f;gak[0].time = 0f;
        gak[1].alpha = 1f;gak[1].time = 1f;

        gradient.SetKeys(gck, gak);
    }

    public void DrawVectorField()
    {
        for(int i = 0; i < startPts.Count; i++)
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

            Color c = gradient.Evaluate(offset.magnitude);
            top.startColor = c;
            top.endColor = c;
            body.startColor = c;
            body.endColor = c;
        }
    }

	// Use this for initialization
	void Start () {

        //shuriken = GetComponent<ParticleSystem>();

        vectors = new List<Transform>();
        startPts = new List<Vector3>();
        offsets = new List<Vector3>();
        max_magnitude = 0f;

        //points = new List<ParticleSystem.Particle>();
        //particles = new ParticleSystem.Particle[shuriken.maxParticles];

        SetGradient();

        SampleVectorField(VectorFieldFunctions.vecFieldFns[(int)sampleFunction],
            -3.5f, 3.5f, -3.5f, 3.5f, -3.5f, 3.5f, 1f);
        DrawVectorField();
        //AnimateParticleFlow();
	}
	
	// Update is called once per frame
	void Update () {
        if(currFn != sampleFunction)
        {
            Clear();
            SampleVectorField(VectorFieldFunctions.vecFieldFns[(int)sampleFunction],
                -3.5f, 3.5f, -3.5f, 3.5f, -3.5f, 3.5f, 1f);
            DrawVectorField();
        }
        //AnimateParticleFlow();
    }

    void Clear()
    {
        foreach(Transform t in vectors)
        {
            Destroy(t.gameObject);
        }
        vectors.Clear();
        startPts.Clear();
        offsets.Clear();
    }


    /*
    * Simulates particle flow with current sampled function
    * Call in Update() for simulation
    */
    void AnimateParticleFlow()
    {
        int count = shuriken.GetParticles(particles);

        for(int i = 0; i < count; i++)
        {
            Vector3 velocity = VectorFieldFunctions.vecFieldFns[(int)sampleFunction](particles[i].position) / max_magnitude / 1f;
            particles[i].velocity = velocity;
            //particles[i].startColor = gradient.Evaluate(velocity.magnitude);
        }

        shuriken.SetParticles(particles, particles.Length);
    }
}
