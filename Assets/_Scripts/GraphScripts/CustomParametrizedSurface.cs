using UnityEngine;
using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Threading;

public class CustomParametrizedSurface : MonoBehaviour
{
    public static CustomParametrizedSurface _instance;
    List<Particle[]> threadResults = new List<Particle[]>();
    public List<ExpressionSet> expressionSets = new List<ExpressionSet>();

    ExpressionSet emptyExprSet;

    #region axis labels
    public AxisLabelManager xAxis;
    public AxisLabelManager yAxis;
    public AxisLabelManager zAxis;
    #endregion

    private Particle[] dest;

    private Coroutine setup;

    public void SetParticleEffect(AnimatedParticleGrapher.ParticleEffectList effect)
    {
        AnimatedParticleGrapher._instance.particleEffect = effect;
    }

    public void SetEffectStrength(float strength)
    {
        AnimatedParticleGrapher._instance.effectStrength = strength;
    }

    public int particleCount;

    AK.ExpressionSolver solver;

    private void Awake()
    {
        _instance = this;
        emptyExprSet = ExpressionSet.CreateEmptySet();
    }

    // Use this for initialization
    protected virtual void Start()
    {
        solver = new AK.ExpressionSolver();
        AnimatedParticleGrapher._instance.ChangeParticleCount(particleCount);
    }

    public MeshFilter mesh;
    Coroutine tessellate;
    public SurfaceTessellation tessel;

    public void CreateExpressionSet()
    {
        expressionSets.Add(new ExpressionSet());
    }

    public void UpdateExpressionSet(List<ExpressionSet> expSet)
    {
        expressionSets = expSet;
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
            float currentScale = CartesianManager._instance.GetScale();

            tessel.EnqueueEquation(currentScale, expressionSet.GetExpression("X").expression, expressionSet.GetExpression("Y").expression,
                                   expressionSet.GetExpression("Z").expression, expressionSet.GetRange("u").Min.Value,
                                   expressionSet.GetRange("u").Max.Value, expressionSet.GetRange("v").Min.Value,
                                   expressionSet.GetRange("v").Max.Value);
        }
    }

    /// <summary>
    /// runs the grapher.
    /// </summary>
    public void GenerateParticles()
    {
        if (setup != null) StopCoroutine(setup);

        if (expressionSets.Count == 0)
        {
            expressionSets.Add(emptyExprSet);
        }

        setup = StartCoroutine(SetupParticles());
    }

    ///<summary>
    ///Changes particle graph and regraphs.
    ///</summary>
    public void ChangeParticleCount(int count)
    {
        particleCount = count;
        AnimatedParticleGrapher._instance.ChangeParticleCount(particleCount);
        GenerateParticles();
    }


    int num_threads;

    private object lck;
    float maxRange;
    List<Particle> dest_list = new List<Particle>();
    int particlesPerES;


    IEnumerator SetupParticles()
    {
        calculating = true;
        lck = new object();
        maxRange = Mathf.NegativeInfinity;

        int totalFrames = 30;
        int defaultParticlesPerES = particleCount / expressionSets.Count;

        //number of frames used for each expression set.
        int framesPerES = totalFrames / expressionSets.Count;

        num_threads = SystemInfo.processorCount;
        int ExcessParticles = 0;
        threadResults = new List<Particle[]>();


        foreach (ExpressionSet es in expressionSets)
        {
            particlesPerES = defaultParticlesPerES + ExcessParticles;
            ExcessParticles = 0;
            //soft copy of the expressionSet. Do not edit actual expressions using this.
            ExpressionSet expressionSet = es.ShallowCopy();
            //remove unused parameters
            string[] keys = new string[expressionSet.GetRangeCount()];
            expressionSet.GetRangeKeys().CopyTo(keys, 0);
            foreach (string op in keys)
            {
                bool used = false;
                foreach (string key in expressionSet.GetExprKeys())
                {
                    if (expressionSet.GetExpression(key).tokens.Contains(op))
                    {
                        used = true;
                    }
                }
                if (!used && expressionSet.GetRangeCount() > 1)
                {
                    expressionSet.RemoveRange(op);
                }
            }

            int depth = expressionSet.GetRangeCount();
            int width = 1;
            if (depth == 0)
            {
                depth = 1;
            }
            else
            {
                width = (int)Mathf.Pow(particlesPerES, 1f / (float)depth);
            }
            List<int[]> samples = SetupSamples(depth, width);
            int thread_chunk = Mathf.CeilToInt((float)samples.Count / (float)num_threads);
            int used_threads = Math.Min(num_threads, samples.Count);
            ExcessParticles += particlesPerES - samples.Count;
            Thread[] threads = new Thread[num_threads];
            int t = 0;
            for (t = 0; t < used_threads; t++)
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
            for (t = 0; t < used_threads; t++)
            {
                threads[t].Join();
            }
            framesPerES = totalFrames / expressionSets.Count;
        }
        CombineResults(ExcessParticles);

        ScaleParticles();

        ScaleCartesian();

        calculating = false;

        AnimatedParticleGrapher._instance.PlotParticles(dest);
    }

    private void CombineResults(int missingParticles)
    {
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
        int brokenParticles = particleCount - temp.Count;
        for (int i = 0; i < brokenParticles; i++)
        {
            temp.Add(lastParticle);
        }
        Debug.Log("Number of misplaced particles: " + brokenParticles);
        dest = temp.ToArray();
    }

    private void ScaleParticles()
    {
        for (int i = 0; i < dest.Length; i++)
        {
            Vector3 pos = (maxRange == 0) ? Vector3.zero : dest[i].position * 10f / maxRange;
            dest[i].position = pos;
            dest[i].color = new Color(Mathf.Pow((pos.x + 10) / 20, 2), Mathf.Pow((pos.y + 10) / 20, 2), Mathf.Pow((pos.z + 10) / 20, 2));
        }
    }

    private void ScaleCartesian()
    {
        CartesianManager._instance.SetScale(maxRange);
    }

    void ThreadedEvaluate(List<int[]> samples, ExpressionSet expressionSet, int TID)
    {
        Dictionary<string, AK.Variable> vars = new Dictionary<string, AK.Variable>();
        int depth = expressionSet.GetRangeCount();
        int width = (int)Mathf.Pow(particlesPerES, 1f / (float)depth);
        System.Random rand = new System.Random();

        AKSolverPackage threadHelper = SetupSolver(expressionSet);

        Dictionary<int, string> indexedParam = new Dictionary<int, string>();
        int k = 0;
        foreach (string name in expressionSet.GetRangeKeys())
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
            if (expressionSet.GetRangeCount() != 0)
            {
                int[] arr = samples[i];
                for (int j = 0; j < depth; j++)
                {
                    int val = arr[j];
                    string name = indexedParam[j];
                    AK.Variable var = vars[name];

                    var.value = threadHelper.parameterMin[name] + (float)val / (width - 1) * (threadHelper.parameterMax[name] - threadHelper.parameterMin[name]);
                }
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

    private struct AKSolverPackage
    {
        public Dictionary<string, float> parameterMin;
        public Dictionary<string, float> parameterMax;
        public List<AK.Expression> expressionList;
        public AK.ExpressionSolver solver;

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

    public float exclusiveModifier = 0.01f;

    private AKSolverPackage SetupSolver(ExpressionSet es)
    {
        AKSolverPackage threadHelper = new AKSolverPackage();
        threadHelper.parameterMin = new Dictionary<string, float>();
        threadHelper.parameterMax = new Dictionary<string, float>();
        threadHelper.solver = new AK.ExpressionSolver();
        threadHelper.expressionList = new List<AK.Expression>();

        foreach (string name in es.GetRangeKeys())
        {
            threadHelper.parameterMin[name] = (float)threadHelper.solver.EvaluateExpression(es.GetRange(name).Min.expression);
            float range = es.GetRange(name).Max.Value - es.GetRange(name).Min.Value;
            if (es.GetRange(name).Min.Exclusive)
            {
                threadHelper.parameterMin[name] += exclusiveModifier * range;
            }
            threadHelper.parameterMax[name] = (float)threadHelper.solver.EvaluateExpression(es.GetRange(name).Max.expression);
            if (es.GetRange(name).Max.Exclusive)
            {
                threadHelper.parameterMax[name] -= exclusiveModifier * range;
            }
            threadHelper.solver.SetGlobalVariable(name, threadHelper.parameterMin[name]);
        }

        foreach (string op in es.GetExprKeys())
        {
            Expression ex = es.GetExpression(op);
            AK.Expression exp = threadHelper.solver.SymbolicateExpression(ex.expression);

            threadHelper.expressionList.Add(exp);

        }

        return threadHelper;
    }
    bool calculating = false;
    public bool isGraphing()
    {
        return !(calculating || AnimatedParticleGrapher._instance.isGraphing());
    }
}