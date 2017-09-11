/**********************************************
* Visualization of two-variable functions using
* particle system w/ several coloring method
**********************************************/

using UnityEngine;
using System.Collections;
using MathFunctions;

public class SurfaceGrapher : MonoBehaviour
{
    public enum ColoringMethod:int
    {
        SOLID = 0,
        GRADIENT,
        CONTOUR,
        DISTANCE,
        NORMAL
    }
    public ColoringMethod coloring;
    private ColoringMethod currColor;

    ParticleSystem shuriken;

    ParticleSystem.Particle[] particles;
    Vector3[] normals;

    [Range(10, 150)]
    public int resolution = 10;
    private int currResolution;

    [Range(-10, 10)]
    public int min_x = -10, min_z = -10;
    private int minX, minZ;
    [Range(-10, 10)]
    public int max_x = 10, max_z = 10;
    private int maxX, maxZ;

    public Gradient gradient;

    private Vector3 front, back, left, right;

    public TwoVariableFunctions.TwoVarFnOptions sampleFunction;
    private TwoVariableFunctions.TwoVarFnOptions currFn;

    // Use this for initialization
    void Start()
    {
        front = new Vector3(0, 0, 0.01f);
        right = new Vector3(0.01f, 0, 0);
        back = new Vector3(0, 0, -0.01f);
        left = new Vector3(-0.01f, 0, 0);

        shuriken = GetComponent<ParticleSystem>();

        particles = new ParticleSystem.Particle[shuriken.main.maxParticles];
        normals = new Vector3[shuriken.main.maxParticles];

        CreateParticles();
        GraphParticles();
        shuriken.SetParticles(particles, particles.Length);
    }

    // Update is called once per frame
    void Update()
    {
        if (currResolution != resolution ||
            minX != min_x || minZ != min_z ||
            maxX != max_x || maxZ != max_z ||
            particles == null)
        {
            CreateParticles();
            GraphParticles();
        }

        if(currFn!= sampleFunction || currColor != coloring)
        {
            GraphParticles();
        }
        shuriken.SetParticles(particles, particles.Length);
    }

    void CreateParticles()
    {
        currResolution = resolution;
        minX = min_x; minZ = min_z;
        maxX = max_x; maxZ = max_z;

        int xSize = max_x - min_x;
        int zSize = max_z - min_z;

        particles = new ParticleSystem.Particle[resolution * resolution];
        normals = new Vector3[resolution * resolution];

        float xOffset = ((float)xSize) / (resolution - 1);
        float zOffset = ((float)zSize) / (resolution - 1);
        int i = 0;
        for (int x = 0; x < resolution; x++)
        {
            for (int z = 0; z < resolution; z++)
            {
                Vector3 pos = new Vector3(x * xOffset + min_x, 0, z * zOffset + min_z);
                particles[i].position = pos;
                particles[i].startColor = new Color((pos.x) / xSize + 0.5f, 0f, (pos.z + 10f) / zSize + 0.5f);
                particles[i++].startSize = Mathf.Sqrt(xOffset * zOffset) * 3.5f;
            }
        }
    }

    void GraphParticles()
    {
        currColor = coloring;
        currFn = sampleFunction;

        float yMax = float.NegativeInfinity;
        float yMin = float.PositiveInfinity;
        float maxMag = float.NegativeInfinity;

        TwoVariableFunctions.TwoVariableFunction fn = 
            TwoVariableFunctions.twoVarFns[(int)sampleFunction];

        float t = Time.timeSinceLevelLoad;

        for (int i = 0; i < particles.Length; i++)
        {
            Vector3 pos = particles[i].position;

            float y = fn(pos.x, pos.z);
            if(y > yMax)
            {
                yMax = y;
            }
            if(y < yMin)
            {
                yMin = y;
            }
            if(pos.magnitude > maxMag)
            {
                maxMag = pos.magnitude;
            }
            particles[i].position = new Vector3(pos.x, y, pos.z);
            if (coloring == ColoringMethod.NORMAL)
            {
                Vector3 f = pos + front;
                Vector3 b = pos + back;
                Vector3 l = pos + left;
                Vector3 r = pos + right;

                f += new Vector3(0, fn(f.x, f.z));
                b += new Vector3(0, fn(b.x, b.z));
                l += new Vector3(0, fn(l.x, l.z));
                r += new Vector3(0, fn(r.x, r.z));

                Vector3 n = Vector3.Cross(r, f);
                n += Vector3.Cross(f, l);
                n += Vector3.Cross(l, b);
                n += Vector3.Cross(b, r);

                normals[i] = n.normalized;
            }
        }
        for(int i = 0; i < particles.Length; i++)
        {
            switch (coloring)
            {
                case ColoringMethod.GRADIENT:
                    float y = particles[i].position.y;
                    particles[i].startColor = gradient.Evaluate((y - yMin) / (yMax - yMin));
                    break;
                case ColoringMethod.CONTOUR:
                    float y0 = particles[i].position.y;
                    float ratio = (y0 - yMin) / (yMax - yMin);
                    if(ratio * 100f % 10f <1.5f || ratio * 100f % 10f > 9.5f)
                    {
                        particles[i].startColor = Color.grey;
                    }
                    else
                    {
                        particles[i].startColor = Color.white;
                    }
                    break;
                case ColoringMethod.DISTANCE:
                    float m = particles[i].position.magnitude;
                    particles[i].startColor = gradient.Evaluate(m / maxMag);
                    break;
                case ColoringMethod.SOLID:
                    particles[i].startColor = Color.cyan;
                    break;
                case ColoringMethod.NORMAL:
                    particles[i].startColor = new Color((normals[i].x + 1f) / 2f,
                        (normals[i].y + 1f) / 2f,
                        (normals[i].z + 1f) / 2f);
                    break;
            }
        }
    }
}
