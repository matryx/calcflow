using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System.Threading;

public class SphericalParametrization : MonoBehaviour {

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

    private Coroutine anim;
    private Coroutine setup;

    private const int PARTICLE_SIZE = 2 * 12 + 16;
    private const int GROUP_SIZE = 256;

    private int particleCount;
    private int threadGroups;

    public enum ParticleEffectList
    {
        None, Gravity, Lerp, SmoothLerp,
        Explode, Swirl
    }
    private string[] kernels =
    {
        "None","Gravity","Lerp",
        "SmoothLerp","Explode","Swirl"
    };
    public ParticleEffectList particleEffect = ParticleEffectList.None;

    [Range(0.1f, 2.0f)] public float effectStrength = 2.0f;

    public int resolution;
    public Gradient gradient;

    AK.ExpressionSolver solver;

    //public ExpressionSet es = new ExpressionSet();
    public string expression;
    string expr;
    public string uMinExpr, uMaxExpr, vMinExpr, vMaxExpr;
    string uminExpr, umaxExpr, vminExpr, vmaxExpr;
    List<Particle> results = new List<Particle>();
    float uMin, uMax, vMin, vMax;

    int thread_num = 1;
    private void Start()
    {
        thread_num = SystemInfo.processorCount;
        //thread_num = 4;
        solver = new AK.ExpressionSolver();
        InitializeParticleSystem();
        StartCoroutine(Evaluate());
    }

    public bool press = false;
    private void Update()
    {
        if (press)
        {
            press = false;
            expr = expression;
            uminExpr = uMinExpr;
            umaxExpr = uMaxExpr;
            vminExpr = vMinExpr;
            vmaxExpr = vMaxExpr;
            StartCoroutine(Evaluate());
        }
    }

    #region GPU particles
    void InitializeParticleSystem()
    {
        particleCount = resolution * resolution;
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

    void FixedUpdate()
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
        Graphics.DrawProceduralNow(MeshTopology.Points, particleCount);
    }
    #endregion

    IEnumerator Evaluate()
    {
        insert_lck = new object();

        if (anim != null) StopCoroutine(anim);
        int chunkSize = resolution / thread_num;
        int remain = resolution % thread_num;
        Thread[] threads = new Thread[thread_num];
        for(int i = 0; i < thread_num; i++)
        {
            int TID = i;
            if (TID + 1 < thread_num)
                threads[TID] = new Thread(() => ThreadedEvaluate(TID, chunkSize, 0));
            else
                threads[TID] = new Thread(() => ThreadedEvaluate(TID, chunkSize, remain));
            threads[TID].Start();
        }
        int waitFrame = 10;
        for(int i = 0; i < waitFrame; i++)
        {
            yield return null;
        }
        for(int i =0;i<thread_num; i++)
        {
            threads[i].Join();
        }

        dest = results.ToArray();

        pBuffer.GetData(particles);
        sBuffer.SetData(particles);
        dBuffer.SetData(dest);
        animProgress = 0;
    }

    object insert_lck;
    void ThreadedEvaluate(int TID, int chunkSize, int extra)
    {
        AK.ExpressionSolver solver_ = new AK.ExpressionSolver();
        AK.Variable u_ = solver_.SetGlobalVariable("theta", 0);
        AK.Variable v_ = solver_.SetGlobalVariable("phi", 0);
        AK.Expression exp_ = solver_.SymbolicateExpression(expr);
        float umin = (float)solver_.EvaluateExpression(uminExpr);
        float umax = (float)solver_.EvaluateExpression(umaxExpr);
        float vmin = (float)solver_.EvaluateExpression(vminExpr);
        float vmax = (float)solver_.EvaluateExpression(vmaxExpr);
        System.Random rand = new System.Random();
        for(int i = 0; i < chunkSize+extra; i++)
        {
            float u = (float)(chunkSize * TID + i) / (resolution - 1);
            u_.value = umin + (umax - umin) * u;
            for (int j = 0; j < resolution; j++)
            {
                float v = (float)j / (resolution - 1);
                v_.value = vmin + (vmax - vmin) * v;
                float result = (float)exp_.Evaluate();
                Vector3 spherical = new Vector3((float)u_.value, (float)v_.value, result);
                Vector3 cartesian = SphericalToCartesian(spherical);
                Vector3 vel = new Vector3(
                        (float)rand.NextDouble(),
                        (float)rand.NextDouble(),
                        (float)rand.NextDouble());
                vel = vel.normalized * 0.1f;
                float sqrt2 = Mathf.Sqrt(2.0f);
                Color c = Color.cyan * Mathf.Sqrt(u * u + v * v) / sqrt2
                    + Color.yellow * (1.0f - Mathf.Sqrt(u * u + v * v) / sqrt2)
                    + Color.red * Mathf.Sqrt(u * u + (1 - v) * (1 - v)) / sqrt2
                    + Color.green * (1.0f - Mathf.Sqrt(u * u + (1 - v) * (1 - v)) / sqrt2);

                Vector3 pos = new Vector3(cartesian.x, cartesian.z, cartesian.y);
                Particle p = new Particle()
                {
                    position = pos,
                    velocity = vel,
                    color = c
                };
                lock (insert_lck)
                {
                    results.Add(p);
                }
            }
        }
    }

    Vector3 SphericalToCartesian(Vector3 spherical)
    {
        float rho = spherical.z; float theta = spherical.x; float phi = spherical.y;
        return new Vector3(
            rho * Mathf.Cos(theta) * Mathf.Sin(phi),
            rho * Mathf.Sin(theta) * Mathf.Sin(phi),
            rho * Mathf.Cos(phi)
            );
    }
}
