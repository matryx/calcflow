using UnityEngine;
using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Threading;
using VoxelBusters.RuntimeSerialization;


//Public variables SHOULD NOT be serialized but must be serialized at this time because of the way 
//that values were assigned through editor. Future iterations should make an effort to not require public serialization.
[RuntimeSerializable(typeof(MonoBehaviour), false, false)]
public class CustomParametrizedSurface : ManualSerializeBehavior
{

    public bool changeTestVals;

    public void LateUpdate()
    {
        if (changeTestVals)
        {
            particleCount = 100;
            changeTestVals = false;
        }
    }

    #region Serializable Fields
    [RuntimeSerializeField]
    public List<ExpressionSet> expressionSets = new List<ExpressionSet>();
    [RuntimeSerializeField]
    public int particleCount = 12;
    //Fields that are serializable on their own.
    [RuntimeSerializeField]
    [Range(0.1f, 2.0f)]
    public float effectStrength = 2.0f;
    [RuntimeSerializeField]
    public float currentScale = 10;
    [RuntimeSerializeField]
    public float particleSize = 0.05f;
    [RuntimeSerializeField]
    public float exclusiveModifier = 0.01f;
    [RuntimeSerializeField]
    private float animProgress; //might be dynamic.
    [RuntimeSerializeField]
    private string[] kernels =
    {
        "None", "Gravity", "Lerp",
        "SmoothLerp", "Explode",
        "Swirl"
    };
    #endregion

    #region Surrogate Fields
    //Fields that are serializable substitutes for non-serializable field. Used to restore those fields in runtime.
    [RuntimeSerializeField]
    public string particleTextureLoc = "Textures/particle";
    [RuntimeSerializeField]
    public string particleShaderLoc = "Shaders/ParticleBillboard";
    [RuntimeSerializeField]
    public string computeShaderLoc = "ComputeShaders/Particle Animations";

    #endregion

    #region non-serializable fields
    //Fields that are not serializable and require a surrogate for loading.
    [NonRuntimeSerializedField]
    public Texture2D particleSprite;
    [NonRuntimeSerializedField]
    public Shader particleShader;
    [NonRuntimeSerializedField]
    public ComputeShader particleAnimation;
    #endregion

    #region Dynamic Fields
    //Fields that do not need to be serialized because they are calculated during runtime.
    private Material particleMaterial;
    List<Particle[]> threadResults = new List<Particle[]>();
    private Particle[] particles;
    private Particle[] source;
    private Particle[] dest;
    private ComputeBuffer pBuffer;
    private ComputeBuffer sBuffer;
    private ComputeBuffer dBuffer;
    private Coroutine anim;
    private Coroutine setup;
    private int threadGroups;
    public ParticleEffectList particleEffect = ParticleEffectList.None;

    #endregion


    #region constants
    const ExpressionSet.ExpOptions X = ExpressionSet.ExpOptions.X;
    const ExpressionSet.ExpOptions Y = ExpressionSet.ExpOptions.Y;
    const ExpressionSet.ExpOptions Z = ExpressionSet.ExpOptions.Z;
    // size of a particle struct in bytes
    [RuntimeSerializeField]
    public const int PARTICLE_SIZE = 2 * 12 + 16;
    // number of threads for a group
    [RuntimeSerializeField]
    public const int GROUP_SIZE = 256;
    #endregion


    protected override void manualSerialize()
    {

    }
    protected override void manualDeserialize()
    {
        // print("this works!");
        // InitializeVariables();
        // InitializeParticleSystem();
        // restoreGradient();
    }
    public override void OnAfterRuntimeSerialize()
    {
    }

    private void restoreGradient()
    {
        Gradient g;
        GradientColorKey[] gck;
        GradientAlphaKey[] gak;
        g = new Gradient();
        gck = new GradientColorKey[5];
        gck[0].color = Color.blue;
        gck[0].time = 0.0F;
        gck[1].color = Color.cyan;
        gck[1].time = 0.25F;
        gck[2].color = Color.green;
        gck[2].time = 0.5F;
        gck[3].color = Color.yellow;
        gck[3].time = 0.75F;
        gck[4].color = Color.red;
        gck[4].time = 1.0F;
        g.colorKeys = gck;
        //g.SetKeys(gck);
        g.mode = GradientMode.Blend;
        gradient = g;
    }

    #region axis labels
    public AxisLabelManager xAxis;
    public AxisLabelManager yAxis;
    public AxisLabelManager zAxis;
    #endregion


    private struct Particle
    {
        public Vector3 position;
        public Vector3 velocity;
        public Color color;
    }

    public enum ParticleEffectList
    {
        None, Gravity, Lerp,
        SmoothLerp, Explode,
        Swirl
    }


    public Gradient gradient;

    AK.ExpressionSolver solver;

    // Use this for initialization
    protected virtual void Start()
    {
        solver = new AK.ExpressionSolver();
        InitializeVariables();
        InitializeParticleSystem();
        restoreGradient();
    }

    private void InitializeVariables()
    {
        particleAnimation = (ComputeShader)Resources.Load(computeShaderLoc);
        particleShader = Resources.Load(particleShaderLoc) as Shader;
        particleSprite = Resources.Load(particleTextureLoc) as Texture2D;
    }

    public MeshFilter mesh;
    Coroutine tessellate;
    public SurfaceTessellation tessel;

    public void CreateExpressionSet()
    {
        expressionSets.Add(new ExpressionSet());
    }

    public void RemoveExpressionSet(int index)
    {
        if (index < expressionSets.Count)
        {
            expressionSets.RemoveAt(index);
        }
    }

    public void GenerateMesh()
    {
        if (tessel != null)
        {
            tessel.gameObject.SetActive(true);
        }
        foreach (ExpressionSet expressionSet in expressionSets)
        {
            tessel.EnqueueEquation(expressionSet.expressions[X].expression, expressionSet.expressions[Y].expression, expressionSet.expressions[Z].expression, expressionSet.ranges["u"].Min.Value, expressionSet.ranges["u"].Max.Value, expressionSet.ranges["v"].Min.Value, expressionSet.ranges["v"].Max.Value);
        }
    }

    void InitializeParticleSystem()
    {
        threadGroups = Mathf.CeilToInt((float)particleCount / GROUP_SIZE);
        print("PC: " + particleCount);
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

        Debug.Log("Creating compute buffers");
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


    /// <summary>
    /// runs the grapher.
    /// </summary>
    public void GenerateParticles()
    {
        if (setup != null) StopCoroutine(setup);
        setup = StartCoroutine(SetupParticles());
    }

    ///<summary>
    ///Changes particle graph and regraphs.
    ///</summary>
    public void ChangeParticleCount(int count)
    {
        particleCount = count;
        releaseBuffers();
        InitializeParticleSystem();
        GenerateParticles();
    }


    int num_threads;

    private object lck;
    float maxRange;
    List<Particle> dest_list = new List<Particle>();
    int particlesPerES;

    bool calculating = false;

    IEnumerator SetupParticles()
    {
        calculating = true;
        lck = new object();
        maxRange = Mathf.NegativeInfinity;

        if (anim != null) StopCoroutine(anim);

        int totalFrames = 30;
        int defaultParticlesPerES = particleCount / expressionSets.Count;
        //number of frames used for each expression set.
        int framesPerES = totalFrames / expressionSets.Count;

        num_threads = SystemInfo.processorCount;
        int missingParticles = 0;
        threadResults = new List<Particle[]>();

        int expressionNumber = 0;

        foreach (ExpressionSet es in expressionSets)
        {
            particlesPerES = defaultParticlesPerES + missingParticles;
            missingParticles = 0;
            expressionNumber++;
            //soft copy of the expressionSet. Do not edit actual expressions using this.
            ExpressionSet expressionSet = es.ShallowCopy();
            //remove unused parameters
            string[] keys = new string[expressionSet.ranges.Keys.Count];
            expressionSet.ranges.Keys.CopyTo(keys, 0);
            foreach (string op in keys)
            {
                bool used = false;
                foreach (ExpressionSet.ExpOptions key in expressionSet.expressions.Keys)
                {
                    if (expressionSet.expressions[key].tokens.Contains(op))
                    {
                        used = true;
                    }
                }
                if (!used && expressionSet.ranges.Count > 1)
                {
                    expressionSet.RemoveRange(op);
                }
            }

            int depth = expressionSet.ranges.Count;
            int width = (int)Mathf.Pow(particlesPerES, 1f / (float)depth);
            List<int[]> samples = SetupSamples(depth, width);
            int thread_chunk = Mathf.CeilToInt(samples.Count / 4);
            missingParticles += particlesPerES - samples.Count;

            Thread[] threads = new Thread[num_threads];
            int t = 0;
            for (t = 0; t < num_threads; t++)
            {
                int tc = t;
                int TIDc = tc;


                threads[t] = new Thread(() => ThreadedEvaluate(samples.GetRange(tc * thread_chunk, Math.Min(thread_chunk, samples.Count - tc * thread_chunk)), expressionSet, TIDc));
                threads[t].Start();
            }
            while (framesPerES-- != 0)
            {
                yield return null;
            }
            for (t = 0; t < num_threads; t++)
            {
                threads[t].Join();
            }
            framesPerES = totalFrames / expressionSets.Count;
        }
        List<Particle> temp = new List<Particle>();
        foreach (Particle[] array in threadResults)
        {
            temp.AddRange(array);
        }
        Particle lastParticle = temp[temp.Count - 1];
        for (int i = 0; i < missingParticles; i++)
        {
            temp.Add(lastParticle);
        }
        dest = temp.ToArray();
        for (int i = 0; i < dest.Length; i++)
        {
            Vector3 pos = (maxRange == 0) ? Vector3.zero : dest[i].position * 10f / maxRange;
            dest[i].position = pos;
            dest[i].color = new Color(Mathf.Pow((pos.x + 10) / 20, 2), Mathf.Pow((pos.y + 10) / 20, 2), Mathf.Pow((pos.z + 10) / 20, 2));
        }

        xAxis.Max = maxRange; xAxis.Min = -maxRange;
        yAxis.Max = maxRange; yAxis.Min = -maxRange;
        zAxis.Max = maxRange; zAxis.Min = -maxRange;

        currentScale = (maxRange == 0) ? 0 : 10 / maxRange;

        pBuffer.GetData(particles);
        sBuffer.SetData(particles);
        dBuffer.SetData(dest);
        animProgress = 0;
        calculating = false;
    }

    internal void ThreadedEvaluate(List<int[]> samples, ExpressionSet expressionSet, int TID)
    {
        Dictionary<string, AK.Variable> vars = new Dictionary<string, AK.Variable>();
        int depth = expressionSet.ranges.Count;
        int width = (int)Mathf.Pow(particlesPerES, 1f / (float)depth);
        System.Random rand = new System.Random();

        ThreadHelper threadHelper = SetupSolver(expressionSet);

        Dictionary<int, string> indexedParam = new Dictionary<int, string>();
        int k = 0;
        foreach (string name in expressionSet.ranges.Keys)
        {
            AK.Variable var = threadHelper.solver.GetGlobalVariable(name);
            indexedParam.Add(k, name);
            vars[name] = var;
            k++;
        }

        int iterations = Math.Min(particlesPerES - samples.Count * TID, samples.Count);
        Particle[] particles = new Particle[iterations];
        for (int i = 0; i < iterations; i++)
        {
            int[] arr = samples[i];
            for (int j = 0; j < depth; j++)
            {
                int val = arr[j];
                string name = indexedParam[j];
                AK.Variable var = vars[name];

                var.value = threadHelper.parameterMin[name] + (float)val / (width - 1) * (threadHelper.parameterMax[name] - threadHelper.parameterMin[name]);
            }

            float x = (float)threadHelper.expressionList[0].Evaluate();
            float z = (float)threadHelper.expressionList[1].Evaluate();
            float y = (float)threadHelper.expressionList[2].Evaluate();

            lock (lck)
            {
                maxRange = (Mathf.Abs(x) > maxRange) ? Mathf.Abs(x) : maxRange;
                maxRange = (Mathf.Abs(y) > maxRange) ? Mathf.Abs(y) : maxRange;
                maxRange = (Mathf.Abs(z) > maxRange) ? Mathf.Abs(z) : maxRange;
            }

            particles[i] = new Particle();
            particles[i].position = new Vector3(x, y, z);
            Vector3 vel = new Vector3();
            vel.x = 0.5f - (float)rand.NextDouble();
            vel.y = 0.5f - (float)rand.NextDouble();
            vel.z = 0.5f - (float)rand.NextDouble();
            vel = 0.1f * Vector3.Normalize(vel);
            particles[i].velocity = vel;
        }
        lock (lck)
        {
            threadResults.Add(particles);
        }
    }

    internal struct ThreadHelper
    {
        internal Dictionary<string, float> parameterMin;
        internal Dictionary<string, float> parameterMax;
        internal List<AK.Expression> expressionList;
        internal AK.ExpressionSolver solver;

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


    private ThreadHelper SetupSolver(ExpressionSet es)
    {
        ThreadHelper threadHelper = new ThreadHelper();
        threadHelper.parameterMin = new Dictionary<string, float>();
        threadHelper.parameterMax = new Dictionary<string, float>();
        threadHelper.solver = new AK.ExpressionSolver();
        threadHelper.expressionList = new List<AK.Expression>();

        foreach (string name in es.ranges.Keys)
        {
            threadHelper.parameterMin[name] = (float)threadHelper.solver.EvaluateExpression(es.ranges[name].Min.expression);
            float range = es.ranges[name].Max.Value - es.ranges[name].Min.Value;
            if (es.ranges[name].Min.Exclusive)
            {
                threadHelper.parameterMin[name] += exclusiveModifier * range;
            }
            threadHelper.parameterMax[name] = (float)threadHelper.solver.EvaluateExpression(es.ranges[name].Max.expression);
            if (es.ranges[name].Max.Exclusive)
            {
                threadHelper.parameterMax[name] -= exclusiveModifier * range;
            }
            threadHelper.solver.SetGlobalVariable(name, threadHelper.parameterMin[name]);
        }

        foreach (ExpressionSet.ExpOptions op in es.expressions.Keys)
        {
            Expression ex = es.expressions[op];
            AK.Expression exp = threadHelper.solver.SymbolicateExpression(ex.expression);

            threadHelper.expressionList.Add(exp);

        }

        return threadHelper;
    }

    public bool isGraphing()
    {
        return !(animProgress == 1 || calculating);
    }

    public override void OnDestroy()
    {
        base.OnDestroy();
        releaseBuffers();

    }
    private void releaseBuffers()
    {  
        Debug.Log("Releasing Buffers");
        pBuffer.Release();
        sBuffer.Release();
        dBuffer.Release();
    }
}