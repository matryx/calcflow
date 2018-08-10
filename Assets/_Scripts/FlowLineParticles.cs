using UnityEngine;
using System.Collections;
using System.Collections.Generic;
using MathFunctions;
using System.Threading;

public class FlowLineParticles : MonoBehaviour
{

    public CustomVectorField vectorField;
    public static FlowLineParticles _instance;

    AK.ExpressionSolver solver;
    AK.Expression expX, expY, expZ;
    AK.Variable varX, varY, varZ;
    string currExpX, currExpY, currExpZ;
    Vector3 currLocalPos;
    public string t_min, t_max;
    bool initialized;

    //ParticleSystem shuriken;
    //List<ParticleSystem.Particle> points;
    //ParticleSystem.Particle[] particles;

    //LinkedList<Vector3> positions;
    SortedDictionary<int, Vector3> positions;
    int MAX_COUNT = 10000;
    [Range(1f, 50000f)] public float step_count;

    float currHighlight = 0f;
    public float time_step = 10f;

    public ConstraintGrabbable referencePoint;

    private struct Particle
    {
        public Vector3 position;
        public Vector3 velocity;
        public Color color;
    }

    private Particle[] particles;
    private Particle[] dest;
    private float animProgress;
    public float particleSize = 0.05f;
    public Texture2D particleSprite;
    public Shader particleShader;
    public ComputeShader particleAnimation;
    private Material particleMaterial;
    private ComputeBuffer pBuffer;
    private ComputeBuffer dBuffer;
    public Transform vfTransform;

    private const int PARTICLE_SIZE = 2 * 12 + 16;
    private const int GROUP_SIZE = 256;

    private int particleCount;
    private int threadGroups;

    float scale;

    [Range(0.1f, 2.0f)] public float effectStrength = 2.0f;

    // Use this for initialization
    void Awake()
    {
        positions = new SortedDictionary<int, Vector3>();
        solver = new AK.ExpressionSolver();
        expX = new AK.Expression();
        expY = new AK.Expression();
        expZ = new AK.Expression();

        sampling = null;
        _instance = this;
    }

    void Start()
    {
        CartesianManager._instance.AddScaleCallback(ScaleUpdated);
    }

    void OnDestroy()
    {
        CartesianManager._instance.RemoveScaleCallback(ScaleUpdated);
    }

    void ScaleUpdated(float newScale)
    {
        scale = newScale;
        ForceUpdate();
    }

    Coroutine sampling;
    Vector3 lastPos;
    bool thread_finished = true;
    bool last_grab_state = false;
    bool need_final_update = false;
    // Update is called once per frame
    void Update()
    {
        if (t_min == "")
        {
            t_min = "0";
        }
        if (t_max == "")
        {
            t_max = "0";
        }

        transform.position = referencePoint.transform.position;

        if ((currLocalPos != referencePoint.lastLocalPos
            || currExpX != vectorField.expressionX
            || currExpY != vectorField.expressionY
            || currExpZ != vectorField.expressionZ)
            && thread_finished == true)
        {
            Thread thread = new Thread(() => SamplePoints());
            thread_finished = false;
            thread.IsBackground = true;
            thread.Start();
        }

        if (need_final_update && thread_finished)
        {
            Thread thread = new Thread(() => SamplePoints());
            need_final_update = false;
            thread_finished = false;
            thread.IsBackground = true;
            thread.Start();
        }


        if (last_grab_state == true && referencePoint.IsGrabbed == false)
        {
            need_final_update = true;
        }

        lastPos = referencePoint.lastLocalPos;
        last_grab_state = referencePoint.IsGrabbed;
    }

    void FixedUpdate()
    {
        RenderParticles();
        Highlight();

        animProgress = Mathf.Clamp(animProgress + effectStrength * 0.005f, 0, 1);

        int kID = particleAnimation.FindKernel("None");
        particleAnimation.Dispatch(kID, threadGroups, 1, 1);
    }

    void OnRenderObject()
    {
        if (!initialized) return;

        particleMaterial.SetPass(0);
        particleMaterial.SetVector("_size", particleSize * transform.lossyScale);
        particleMaterial.SetVector("_worldPos", transform.position);

        var m = Matrix4x4.TRS(Vector3.zero, transform.rotation, transform.lossyScale);
        particleMaterial.SetMatrix("_worldRot", m);
        Graphics.DrawProcedural(MeshTopology.Points, particleCount);
    }

    public void ForceUpdate()
    {
        SamplePoints();
    }

    private object lck;
    AK.Expression[] expXs, expYs, expZs;
    AK.ExpressionSolver[] solvers;
    int thread_num;
    void SamplePoints()
    {
        float tmin;
        float tmax;

        Vector3 start = referencePoint.lastLocalPos;
        ExpressionSet vecES = vectorField.es;
        if (vecES == null)
        {
            vecES = ExpressionSet.CreateEmptySet();
        }
        //lck = new object();
        //thread_num = SystemInfo.processorCount;

        //thread_num = 1;
        //thread_num = (thread_num > 2) ? 2 : thread_num;

        if (t_min == "")
        {
            t_min = "0";
        }
        if (t_max == "")
        {
            t_max = "0";
        }

        tmin = (float)solver.EvaluateExpression(t_min);
        tmin = (tmin > 0) ? 0 : tmin;
        tmax = (float)solver.EvaluateExpression(t_max);
        tmax = (tmax < 0) ? 0 : tmax;

        currLocalPos = referencePoint.lastLocalPos;
        currExpX = vectorField.expressionX;
        currExpY = vectorField.expressionY;
        currExpZ = vectorField.expressionZ;

        int lastCount = positions.Count;
        positions.Clear();

        solver = new AK.ExpressionSolver();

        solver.SetGlobalVariable("x", referencePoint.lastLocalPos.x * 10 / scale);
        solver.SetGlobalVariable("y", referencePoint.lastLocalPos.y * 10 / scale);
        solver.SetGlobalVariable("z", referencePoint.lastLocalPos.z * 10 / scale);
        expX = solver.SymbolicateExpression(vecES.GetExpression("X").expression);
        expY = solver.SymbolicateExpression(vecES.GetExpression("Y").expression);
        expZ = solver.SymbolicateExpression(vecES.GetExpression("Z").expression);
        varX = solver.GetGlobalVariable("x");
        varY = solver.GetGlobalVariable("y");
        varZ = solver.GetGlobalVariable("z");

        //solvers = new AK.ExpressionSolver[thread_num];
        //expXs = new AK.Expression[thread_num];
        //expYs = new AK.Expression[thread_num];
        //expZs = new AK.Expression[thread_num];
        //for(int i = 0; i < thread_num; i++)
        //{
        //    solvers[i] = new AK.ExpressionSolver();
        //    solvers[i].SetGlobalVariable("x", 0);
        //    solvers[i].SetGlobalVariable("y", 0);
        //    solvers[i].SetGlobalVariable("z", 0);
        //    expXs[i] = solvers[i].SymbolicateExpression(vectorField.es.expressions[ExpressionSet.ExpOptions.X].expression);
        //    expYs[i] = solvers[i].SymbolicateExpression(vectorField.es.expressions[ExpressionSet.ExpOptions.Y].expression);
        //    expZs[i] = solvers[i].SymbolicateExpression(vectorField.es.expressions[ExpressionSet.ExpOptions.Z].expression);
        //}
        //Thread[] threads = new Thread[thread_num];
        float positiveCount = tmax / time_step;
        positiveCount = (positiveCount > 50000) ? 50000 : positiveCount;
        float negativeCount = -tmin / time_step;
        negativeCount = (negativeCount > 50000) ? 50000 : negativeCount;
        //Vector3[] startPts = new Vector3[thread_num];
        //startPts[0] = referencePoint.lastLocalPos;
        //for(int i = 1; i < thread_num; i++)
        //{
        //    startPts[i] = RK4(startPts[i - 1], time_step);
        //}
        //for(int i = 0; i < thread_num; i++)
        //{
        //    int index = i;
        //    threads[i] = new Thread(() => ThreadedSampling(index, startPts[index], time_step * thread_num, positiveCount / thread_num,
        //        negativeCount / thread_num));
        //    threads[i].Start();
        //}
        ////for (int i = 0; i < 5; i++)
        ////{
        ////    yield return null;
        ////}
        //for (int i = 0; i < thread_num; i++)
        //{
        //    threads[i].Join();
        //}

        Vector3 curr = start;
        for (int i = 0; i < positiveCount; i++)
        {
            curr = RK4(curr, time_step);
            positions.Add(i + 1, curr);
        }
        curr = start;
        for (int i = 0; i < negativeCount; i++)
        {
            curr = RK4(curr, -time_step);
            positions.Add(-i, curr);
        }

        if (positions.Count != lastCount)
        {
            InitializeParticleSystem();
        }

        //RenderParticles();

        currHighlight = 0;
        thread_finished = true;
    }

    /// <summary>
    /// vectorized 4th order Runge-Kutta ODE solver for vector field
    /// </summary>
    /// <param name="v0"></param>
    /// <param name="dt"></param>
    /// <returns></returns>
    Vector3 RK4(Vector3 v0, float dt)
    {
        varX.value = v0.x; varY.value = v0.z; varZ.value = v0.y;
        Vector3 m1 = new Vector3((float)expX.Evaluate(), (float)expZ.Evaluate(), (float)expY.Evaluate()).normalized;
        varX.value = v0.x + m1.x * dt / 2.0f; varY.value = v0.z + m1.z * dt / 2.0f; varZ.value = v0.y + m1.y * dt / 2.0f;
        Vector3 m2 = new Vector3((float)expX.Evaluate(), (float)expZ.Evaluate(), (float)expY.Evaluate()).normalized;
        varX.value = v0.x + m2.x * dt / 2.0f; varY.value = v0.z + m2.z * dt / 2.0f; varZ.value = v0.y + m2.y * dt / 2.0f;
        Vector3 m3 = new Vector3((float)expX.Evaluate(), (float)expZ.Evaluate(), (float)expY.Evaluate()).normalized;
        varX.value = v0.x + m3.x * dt; varY.value = v0.z + m3.z * dt; varZ.value = v0.y + m3.y * dt;
        Vector3 m4 = new Vector3((float)expX.Evaluate(), (float)expZ.Evaluate(), (float)expY.Evaluate()).normalized;
        Vector3 m = (m1 + m2 * 2.0f + m3 * 2.0f + m4) / 6.0f;
        return v0 + m * dt;
    }

    void RenderParticles()
    {
        Vector3[] posArray = new Vector3[positions.Count];
        positions.Values.CopyTo(posArray, 0);
        int index = 0;
        for (int i = 0; i < positions.Count; i++)
        {
            dest[index++] = new Particle()
            {
                position = transform.InverseTransformPoint(transform.parent.TransformPoint(posArray[i])),
                velocity = Vector3.zero,
                color = Color.white
            };
        }
    }

    void Highlight()
    {
        if (dest == null) return;
        for (int i = 0; i < dest.Length; i++)
        {
            if (i > currHighlight && i < currHighlight + 40)
            {
                dest[i].color = Color.red;
            }
            else
            {
                dest[i].color = Color.white;
            }
        }
        currHighlight += 4;
        if (currHighlight > dest.Length)
        {
            currHighlight = 0;
        }
        pBuffer.GetData(particles);
        dBuffer.SetData(dest);
    }

    void InitializeParticleSystem()
    {
        particleCount = positions.Count;
        threadGroups = Mathf.CeilToInt((float)particleCount / GROUP_SIZE);

        int l = particleCount;
        if (l == 0)
        {
            initialized = false;
            return;
        }
        particles = new Particle[l];
        dest = new Particle[l];

        for (int i = 0; i < l; i++)
        {
            //Vector3 pos = UnityEngine.Random.onUnitSphere;
            Vector3 pos = Vector3.zero;
            Color col = new Color(1, 1, 1);

            particles[i].color = col;
            dest[i].color = col;

            particles[i].position = pos;
            dest[i].position = pos;
        }

        pBuffer = new ComputeBuffer(l, PARTICLE_SIZE);
        dBuffer = new ComputeBuffer(l, PARTICLE_SIZE);

        pBuffer.SetData(particles);
        dBuffer.SetData(dest);
        int kID = particleAnimation.FindKernel("None");
        particleAnimation.SetBuffer(kID, "particles", pBuffer);
        particleAnimation.SetBuffer(kID, "dest", dBuffer);
        particleMaterial = new Material(particleShader);
        particleMaterial.SetTexture("_Sprite", particleSprite);
        particleMaterial.SetBuffer("particles", pBuffer);
        initialized = true;
    }
}
