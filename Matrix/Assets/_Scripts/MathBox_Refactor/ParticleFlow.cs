using UnityEngine;
using System.Collections;
using System.Collections.Generic;

public class ParticleFlow : MonoBehaviour {

    ParticleSystem shuriken;
    public Gradient gradient;

    //List<MathBox_Vector> vectors;
    //List<Transform> vectors;
    List<Vector3> startPts;
    List<Vector3> offsets;
    float max_magnitude;

    ParticleSystem.Particle[] particles;
    //List<ParticleSystem.Particle> points;
    int resolution = 1000;

    public delegate Vector3 VectorFunction(float x, float y, float z);
    VectorFunction sampleFn;

    public void SampleVectorField(VectorFunction myFn, float x_min, float x_max, float y_min, float y_max, float z_min, float z_max, float delta)
    {

        max_magnitude = 0f;

        for (float x_temp = x_min; x_temp <= x_max; x_temp += delta)
        {
            for (float y_temp = y_min; y_temp <= y_max; y_temp += delta)
            {
                for (float z_temp = z_min; z_temp <= z_max; z_temp += delta)
                {
                    Vector3 result = myFn(x_temp, y_temp, z_temp);

                    Vector3 target = new Vector3(x_temp, y_temp, z_temp);
                    Vector3 direction = result.normalized;
                    float length = result.magnitude;
                    if (length > max_magnitude)
                    {
                        max_magnitude = length;
                    }
                    startPts.Add(target);
                    offsets.Add(direction * length);

                    //                    Vector3 offset = direction * length / 25f;
                    //                    Vector3 tip = offset * 0.4f;
                    //
                    //                    Transform l = Instantiate(vPrefab);
                    //                    l.SetParent(transform, false);
                    //                    LineRenderer top = l.FindChild("Top").GetComponent<LineRenderer>();
                    //                    top.SetPosition(0, target + offset - tip);
                    //                    top.SetPosition(1, target + offset);
                    //                    LineRenderer body = l.FindChild("Body").GetComponent<LineRenderer>();
                    //                    body.SetPosition(0, target + offset - tip);
                    //                    body.SetPosition(1, target);
                    //                    vectors.Add(l);
                }
            }
        }
    }

    void SetGradient()
    {
        GradientColorKey[] gck = new GradientColorKey[3];
        gck[0].color = Color.blue; gck[0].time = 0f;
        gck[1].color = Color.green; gck[1].time = 0.5f;
        gck[2].color = Color.red; gck[2].time = 1f;
        GradientAlphaKey[] gak = new GradientAlphaKey[2];
        gak[0].alpha = 1f; gak[0].time = 0f;
        gak[1].alpha = 1f; gak[1].time = 1f;

        gradient.SetKeys(gck, gak);
    }

    // Use this for initialization
    void Start()
    {
        resolution = 5000;

        shuriken = GetComponent<ParticleSystem>();
        particles = new ParticleSystem.Particle[shuriken.main.maxParticles];

        //vectors = new List<Transform>();
        startPts = new List<Vector3>();
        offsets = new List<Vector3>();
        max_magnitude = 0f;

        //points = new List<ParticleSystem.Particle>();

        SetGradient();

        //sampleFn = MyFn3;
        sampleFn = MyFn4;

        SampleVectorField(sampleFn, -4f, 4f, -4f, 4f, -4f, 4f, 1f);
        SetParticles();
    }

    // Update is called once per frame
    void Update()
    {
        //AnimateParticles();
        SetParticles();
    }

    public Vector3 MyFn1(float x, float y, float z)
    {
        return new Vector3(x, y, z);
    }

    public Vector3 MyFn2(float x, float y, float z)
    {
        //float x0 = z / y;
        float x0 = (y == 0) ? 0f : z / y;
        float y0 = 0;
        //float z0 = -x / y;
        float z0 = (y == 0) ? 0f : -x / y;
        return new Vector3(x0, y0, z0);
    }

    public Vector3 MyFn3(float x, float y, float z)
    {
        float x0 = y * z;
        float y0 = x * z;
        float z0 = x * y;
        return new Vector3(x0, y0, z0);
    }

    public Vector3 MyFn4(float x, float y, float z)
    {
        return new Vector3(x * x, y * y, z * z);
    }

    public Vector3 MyFn5(float x, float y, float z)
    {
        float x0 = 3f * Mathf.Cos(x) + Mathf.Cos(x) * Mathf.Cos(z);
        float z0 = 3f * Mathf.Sin(x) + Mathf.Sin(x) * Mathf.Cos(z);
        float y0 = Mathf.Sin(z);
        return new Vector3(x0, y0, z0);
    }

    void SetParticles()
    {
        int count = shuriken.GetParticles(particles);
        for (int i = 0; i < count; i++)
        {
            Vector3 pPos = particles[i].position;
            Vector3 v = sampleFn(pPos.x, pPos.y, pPos.z) / max_magnitude /2f;
            particles[i].velocity = v;
        }
        shuriken.SetParticles(particles, count);
    }

    void AnimateParticles()
    {
        shuriken.GetParticles(particles);

        for (int i = 0; i < particles.Length; i++)
        {
            Vector3 currPos = particles[i].position;
            if (currPos.magnitude > 10f)
            {
                Vector3 rand = UnityEngine.Random.insideUnitSphere;
                rand *= 9f;
                particles[i].position = rand;
            }
            Vector3 velocity = sampleFn(particles[i].position.x, particles[i].position.y, particles[i].position.z).normalized;
            //particles[i].velocity = velocity;
            //particles[i].startSize = 0.1f;
            //particles[i].startColor = Color.cyan;
            //particles[i].position += velocity * 0.02f;

            //if (particles[i].position.magnitude <= 3)
            //{
            //    particles[i].startColor = Color.blue;
            //}
            //else if (particles[i].position.magnitude > 3 && particles[i].position.magnitude <= 7)
            //{
            //    particles[i].startColor = Color.yellow;
            //}
            //else
            //{
            //    particles[i].startColor = Color.red;
            //}
            Color c = Color.Lerp(new Color(0, 0, 1), new Color(1, 1, 0), particles[i].position.magnitude / 9f);
            particles[i].startColor = c;
            //particles[i].startColor = gradient.Evaluate(particles[i].position.magnitude / 10f);
            particles[i].startColor = gradient.Evaluate((particles[i].position.x + particles[i].position.y + particles[i].position.z + 11f) / 22f);
        }

        shuriken.SetParticles(particles, particles.Length);
    }
}
