using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using AK;

public class TripleIntegralRegion : MonoBehaviour {

    private struct Particle
    {
        public Vector3 position;
        public Vector3 velocity;
        public Color color;
    }
    private Particle[] particles;
    private Particle[] source;
    private Particle[] dest;
    private float animProgress;

    public float particleSize = 0.05f;
    public Texture2D particleSprite;
    public Shader particleShader;
    public ComputeShader particleAnimation;

    private Material particleMaterial;
    private ComputeBuffer pBuffer;
    private ComputeBuffer sBuffer;
    private ComputeBuffer dBuffer;

    // size of a particle struct in bytes
    private const int PARTICLE_SIZE = 2 * 12 + 16;
    // number of threads for a group
    private const int GROUP_SIZE = 256;
    // num of samples on each axis
    public int resolution = 200;
    private int particleCount;
    private int threadGroups;

    [Range(0.1f, 2.0f)]
    public float effectStrength = 2.0f;

    public enum ParticleEffectList
    {
        None, Gravity, Lerp, SmoothLerp, Explode, Swirl
    }
    private string[] kernels =
    {
        "None", "Gravity", "Lerp", "SmoothLerp", "Explode", "Swirl"
    };
    public ParticleEffectList particleEffect = ParticleEffectList.None;

    ExpressionSolver solver;
    public string expressionXMin, expressionXMax, expressionYMin, expressionYMax, expressionZMin, expressionZMax;
    Variable varX, varY;
    AK.Expression expYMin, expYMax, expZMin, expZMax;
    float xMin, xMax, yMin, yMax, zMin, zMax;

    private void Start()
    {
        solver = new ExpressionSolver();
        InitializeParticleSystem();
        varX = solver.SetGlobalVariable("x", 0);
        varY = solver.SetGlobalVariable("y", 0);
        xMin = (float)solver.EvaluateExpression(expressionXMin);
        xMax = (float)solver.EvaluateExpression(expressionXMax);
        expYMin = solver.SymbolicateExpression(expressionYMin);
        expYMax = solver.SymbolicateExpression(expressionYMax);
        expZMin = solver.SymbolicateExpression(expressionZMin);
        expZMax = solver.SymbolicateExpression(expressionZMax);
        Generate3DRegion();
    }

    private void FixedUpdate()
    {
        animProgress = Mathf.Clamp(animProgress + effectStrength * 0.005f, 0, 1);
        int kID = particleAnimation.FindKernel(kernels[(int)particleEffect]);
        particleAnimation.SetFloat("animProgress", animProgress);
        particleAnimation.SetFloat("strength", effectStrength);
        particleAnimation.SetFloat("time", Time.time);
        particleAnimation.Dispatch(kID, threadGroups, 1, 1);
    }
    
    void OnRenderObject()
    {
        particleMaterial.SetPass(0);
        particleMaterial.SetVector("_size", particleSize * transform.lossyScale);
        particleMaterial.SetVector("_worldPos", transform.position);

        var m = Matrix4x4.TRS(Vector3.zero, transform.rotation, transform.lossyScale);
        particleMaterial.SetMatrix("_worldRot", m);
        Graphics.DrawProcedural(MeshTopology.Points, particleCount);
    }

    private void OnApplicationQuit()
    {
        pBuffer.Release();
        sBuffer.Release();
        dBuffer.Release();
    }

    public void UpdateEquation()
    {
        try
        {
            xMin = (float)solver.EvaluateExpression(expressionXMin);
            xMax = (float)solver.EvaluateExpression(expressionXMax);
            expYMin = solver.SymbolicateExpression(expressionYMin);
            expYMax = solver.SymbolicateExpression(expressionYMax);
            expZMin = solver.SymbolicateExpression(expressionZMin);
            expZMax = solver.SymbolicateExpression(expressionZMax);
        }
        catch (AK.ESSyntaxErrorException e)
        {
            return;
        }
        Generate3DRegion();
    }

    void InitializeParticleSystem()
    {
        particleCount = resolution * resolution * resolution;
        threadGroups = Mathf.CeilToInt((float)particleCount / GROUP_SIZE);

        int l = particleCount;

        particles = new Particle[l];
        source = new Particle[l];
        dest = new Particle[l];

        for (int i = 0; i < l; i++)
        {
            Vector3 pos = UnityEngine.Random.onUnitSphere;
            Color col = new Color(1, 1, 1);

            particles[i].color = col;
            source[i].color = col;
            dest[i].color = col;

            particles[i].position = pos;
            source[i].position = pos;
            dest[i].position = pos;
        }

        pBuffer = new ComputeBuffer(l, PARTICLE_SIZE);
        sBuffer = new ComputeBuffer(l, PARTICLE_SIZE);
        dBuffer = new ComputeBuffer(l, PARTICLE_SIZE);

        pBuffer.SetData(particles);
        sBuffer.SetData(source);
        dBuffer.SetData(dest);

        foreach (string effect in kernels)
        {
            int kID = particleAnimation.FindKernel(effect);
            particleAnimation.SetBuffer(kID, "particles", pBuffer);
            particleAnimation.SetBuffer(kID, "source", sBuffer);
            particleAnimation.SetBuffer(kID, "dest", dBuffer);
        }

        particleMaterial = new Material(particleShader);
        particleMaterial.SetTexture("_Sprite", particleSprite);
        particleMaterial.SetBuffer("particles", pBuffer);
    }

    void Generate3DRegion()
    {
        int i, j, k;
        float x, y, z;
        int index = 0;
        for(i = 0; i < resolution; i++)
        {
            varX.value = x = (float)i / (resolution - 1) * (xMax - xMin) + xMin;
            for(j = 0; j < resolution; j++)
            {
                yMin = (float)expYMin.Evaluate();
                yMax = (float)expYMax.Evaluate();
                varY.value = y = (float)j / (resolution - 1) * (yMax - yMin) + yMin;
                for (k = 0; k < resolution; k++)
                {
                    zMin = (float)expZMin.Evaluate();
                    zMax = (float)expZMax.Evaluate();
                    z = (float)k / (resolution - 1) * (zMax - zMin) + zMin;
                    dest[index].position = new Vector3(x, z, y);
                    //dest[index].color = new Color(Mathf.Pow((float)i / resolution, 2),
                    //    Mathf.Pow((float)j / resolution, 2), Mathf.Pow((float)k / resolution, 2));
                    dest[index].color = new Color(0, 0.3f, 1.0f);
                    if (((i == 0 || i == resolution - 1) && (j == 0 || j == resolution - 1))
                        || ((i == 0 || i == resolution - 1) && (k == 0 || k == resolution - 1))
                        || ((k == 0 || k == resolution - 1) && (j == 0 || j == resolution - 1)))
                    {
                        dest[index].color = new Color(1, 1, 1);
                    }
                    index++;
                }
            }
        }
        pBuffer.GetData(particles);
        sBuffer.SetData(particles);
        dBuffer.SetData(dest);
        animProgress = 0;
    }
}
