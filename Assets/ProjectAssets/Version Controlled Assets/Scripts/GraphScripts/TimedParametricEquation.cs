using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using AK;
using System.Threading;

/// <summary>
/// class for parametric visualization that supports time lapse
/// </summary>
public class TimedParametricEquation : MonoBehaviour
{

    #region GPUParticle
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

    // size of a particle struct in bytes
    private const int PARTICLE_SIZE = 2 * 12 + 16;
    // number of threads for a group
    private const int GROUP_SIZE = 256;

    private int particleCount;
    private int threadGroups;

    public enum ParticleEffectList
    {
        None, Gravity, Lerp,
        SmoothLerp, Explode,
        Swirl
    }
    private string[] kernels =
    {
        "None", "Gravity", "Lerp",
        "SmoothLerp", "Explode",
        "Swirl"
    };
    public ParticleEffectList particleEffect = ParticleEffectList.None;

    [Range(0.1f, 2.0f)]
    public float effectStrength = 2.0f;
    #endregion

    public int resolution;

    private int core_num;

    #region SolverWrapper
    private ExpressionSolver solver;
    internal struct ParametricEquationSet
    {
        internal AK.Expression xExpr;
        internal Dictionary<string, AK.Variable> xVars;
        internal AK.Expression yExpr;
        internal Dictionary<string, AK.Variable> yVars;
        internal AK.Expression zExpr;
        internal Dictionary<string, AK.Variable> zVars;
    }
    private Dictionary<string, AK.Variable> globalVars;
    #endregion

    private int width;
    private int depth;

    private List<int[]> samples;
    //private List<Vector3> results;

    private object lck;
    private float maxRange;
    private List<Particle> results;

    #region EditorTest
    public string ExpressionX;
    public string ExpressionY;
    public string ExpressionZ;
    public float UMin;
    public float UMax;
    public float VMin;
    public float VMax;
    //public float WMin;
    //public float WMax;
    public float TMin;
    public float TMax;
    #endregion
    private float currTime;
    private float lastTime;
    public bool Play;
    private int thread_finished = 0;

    // Use this for initialization
    void Start()
    {
        InitializeParticleSystem();
        core_num = SystemInfo.processorCount;
        lck = new object();
        solver = new ExpressionSolver();
        globalVars = new Dictionary<string, Variable>();
        globalVars["t"] = solver.SetGlobalVariable("t", TMin);
        depth = 2;
        width = (int)Mathf.Pow(particleCount, 1f / (float)depth);
        //GenerateParticles();
        currTime = TMin;
        Play = true;
        StartCoroutine(TimeLapse());
    }

    // Update is called once per frame
    void Update()
    {

    }

    #region ParticleFunctions
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
        if (particleEffect == ParticleEffectList.None) animProgress = 1;
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
    #endregion

    private IEnumerator TimeLapse()
    {
        thread_finished = core_num;
        while (Play)
        {
            if (thread_finished == core_num)
            {
                GenerateParticles();
                lastTime = currTime;
            }
            yield return new WaitForSeconds(0.1f);
            //for(int i = 0; i < 10; i++)
            //{
            //    yield return null;
            //}
            if (thread_finished == core_num)
            {
                //dest = results.ToArray();
                pBuffer.GetData(particles);
                sBuffer.SetData(particles);
                dBuffer.SetData(dest);
                animProgress = 0;
            }
            else
            {
                continue;
            }
            currTime = Mathf.Lerp(TMin, TMax, (lastTime - TMin) / (TMax - TMin)+0.01f);
            if (currTime >= TMax)
                currTime = TMin;
            globalVars["t"].value = currTime;
        }
    }

    public List<int[]> SetupSamples(int depth, int width)
    {
        List<int[]> samples = new List<int[]>();

        bool done = false;
        int[] array = new int[depth];
        while (!done)
        {
            samples.Add(array.Clone() as int[]);
            if (depth > 0)
                array[0] += 1;
            for (int i = 0; i < depth; i++)
            {
                if (array[i] >= width)
                {
                    if (i + 1 == depth)
                    {
                        done = true;
                        break;
                    }
                    array[i] = 0;
                    array[i + 1] += 1;
                }
            }
        }
        return samples;
    }

    public void GenerateParticles()
    {
        samples = SetupSamples(depth, width);
        results = new List<Particle>();
        thread_finished = 0;
        ParametricEquationSet[] pesArr = new ParametricEquationSet[core_num];
        for (int i = 0; i < core_num; i++)
        {
            pesArr[i] = new ParametricEquationSet
            {
                xExpr = new AK.Expression(),
                xVars = new Dictionary<string, Variable>(),
                yExpr = new AK.Expression(),
                yVars = new Dictionary<string, Variable>(),
                zExpr = new AK.Expression(),
                zVars = new Dictionary<string, Variable>()
            };
            pesArr[i].xVars["u"] = pesArr[i].xExpr.SetVariable("u", 0);
            pesArr[i].xVars["v"] = pesArr[i].xExpr.SetVariable("v", 0);
            string[] localVars = new string[depth];
            pesArr[i].xVars.Keys.CopyTo(localVars, 0);
            pesArr[i].xExpr = solver.SymbolicateExpression(ExpressionX, localVars);
            pesArr[i].xVars["u"] = pesArr[i].xExpr.GetVariable("u");
            pesArr[i].xVars["v"] = pesArr[i].xExpr.GetVariable("v");

            pesArr[i].yVars["u"] = pesArr[i].yExpr.SetVariable("u", 0);
            pesArr[i].yVars["v"] = pesArr[i].yExpr.SetVariable("v", 0);
            pesArr[i].yVars.Keys.CopyTo(localVars, 0);
            pesArr[i].yExpr = solver.SymbolicateExpression(ExpressionY, localVars);
            pesArr[i].yVars["u"] = pesArr[i].yExpr.GetVariable("u");
            pesArr[i].yVars["v"] = pesArr[i].yExpr.GetVariable("v");

            pesArr[i].zVars["u"] = pesArr[i].zExpr.SetVariable("u", 0);
            pesArr[i].zVars["v"] = pesArr[i].zExpr.SetVariable("v", 0);
            pesArr[i].zVars.Keys.CopyTo(localVars, 0);
            pesArr[i].zExpr = solver.SymbolicateExpression(ExpressionZ, localVars);
            pesArr[i].zVars["u"] = pesArr[i].zExpr.GetVariable("u");
            pesArr[i].zVars["v"] = pesArr[i].zExpr.GetVariable("v");
        }

        Thread[] threads = new Thread[core_num];
        int count = samples.Count;
        for (int i = 0; i < core_num; i++)
        {
            int TID = i;
            if (i + 1 == core_num)
            {
                threads[TID] = new Thread(() => ThreadedEvaluate(pesArr[TID],
                Mathf.CeilToInt((float)TID / core_num * count),
                count));
            }
            else
            {
                threads[TID] = new Thread(() => ThreadedEvaluate(pesArr[TID],
                    Mathf.CeilToInt((float)TID / core_num * count),
                    Mathf.CeilToInt((float)(TID + 1) / core_num * count)));
            }
            threads[TID].Start();
        }
        //for (int i = 0; i < core_num; i++)
        //{
        //    threads[i].Join();
        //}
        //dest = results.ToArray();
        //pBuffer.GetData(particles);
        //sBuffer.SetData(particles);
        //dBuffer.SetData(dest);
        //animProgress = 0;
    }

    private void ThreadedEvaluate(ParametricEquationSet pes, int begin, int end)
    {
        for (int i = begin; i < end; i++)
        {
            int[] currSample = samples[i];

            pes.xVars["u"].value = UMin + (float)currSample[0] / (width - 1) * (UMax - UMin);
            pes.xVars["v"].value = VMin + (float)currSample[1] / (width - 1) * (VMax - VMin);
            //pes.xVars["w"].value = WMin + (float)currSample[2] / (width - 1) * (WMax - WMin);
            float x = (float)pes.xExpr.Evaluate();

            pes.yVars["u"].value = UMin + (float)currSample[0] / (width - 1) * (UMax - UMin);
            pes.yVars["v"].value = VMin + (float)currSample[1] / (width - 1) * (VMax - VMin);
            //pes.yVars["w"].value = WMin + (float)currSample[2] / (width - 1) * (WMax - WMin);
            float y = (float)pes.yExpr.Evaluate();

            pes.zVars["u"].value = UMin + (float)currSample[0] / (width - 1) * (UMax - UMin);
            pes.zVars["v"].value = VMin + (float)currSample[1] / (width - 1) * (VMax - VMin);
            //pes.zVars["w"].value = WMin + (float)currSample[2] / (width - 1) * (WMax - WMin);
            float z = (float)pes.zExpr.Evaluate();

            lock (lck)
            {
                maxRange = (Mathf.Abs(x) > maxRange) ? Mathf.Abs(x) : maxRange;
                maxRange = (Mathf.Abs(y) > maxRange) ? Mathf.Abs(y) : maxRange;
                maxRange = (Mathf.Abs(z) > maxRange) ? Mathf.Abs(z) : maxRange;
            }

            System.Random rand = new System.Random();
            Particle p = new Particle
            {
                position = new Vector3(x, z, y),
                color = Color.cyan,
                velocity = new Vector3(0.5f - (float)rand.NextDouble(),
                0.5f - (float)rand.NextDouble(),
                0.5f - (float)rand.NextDouble()).normalized * 0.1f
            };

            lock (lck)
            {
                dest[i] = p;
            }
        }
        lock (lck)
        {
            thread_finished++;
        }
    }
}
