using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System;
using System.Text;
using UnityEngine.Networking;
using Nanome.Core;
using Nanome.Maths.Serializers.JsonSerializer;

public class ScatterChart : MonoBehaviour
{

    private struct Particle
    {
        public Vector3 position;
        public Vector3 velocity;
        public Color color;
    }

    private struct Distance
    {
        public Particle point1;
        public Particle point2;
        public float distance;
    }

    private Particle[] particles;
    private Particle[] source;
    private Particle[] dest;

    private List<Particle> temp = new List<Particle>();

    private int threadGroups;
    private Material particleMaterial;
    private ComputeBuffer pBuffer;
    private ComputeBuffer sBuffer;
    private ComputeBuffer dBuffer;
    public float particleSize = 0.02f;
    public Texture2D particleSprite;
    public Shader particleShader;
    public ComputeShader particleAnimation;
    private const int PARTICLE_SIZE = 2 * 12 + 16;
    private const int GROUP_SIZE = 256;
    private float STEP_SIZE = .01f;
    private float animProgress;
    public float currentScale = 10;
    public ParticleEffectList particleEffect = ParticleEffectList.SmoothLerp;
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


    //public int particleCount;
    public Gradient gradient;

    float maxRange = 10;
    bool calculating = false;

    int particleCount = 20000;
    int[] additonalParticles;

    Distance[] distances;

    private static ScatterChart _instance;
    public static ScatterChart GetInstance()
    {
        return _instance;
    }

    private string URL;
    public void SetURL(string newURL)
    {
        Debug.Log("URL SET TO: " + newURL);
        URL = newURL;
    }

    // Use this for initialization
    string output;
    StringBuilder builder = new StringBuilder();
    public List<string> times = new List<string>();
    public List<double> prices = new List<double>();

    public List<GameObject> pointList = new List<GameObject>();
    float[] scaledPrices;

    float max, min;
    int maxI, minI;

    GameObject frameObj;
    LineRenderer frameLine;

    // The factor by which number of points are determined. 1 = full points = lag
    public int numPoints = 3;

    // Higher resolution will stretch out the graph, lower will compress it.
    float resolution = 25f;
    //public int numPoints = 10;
    public Material frameMaterial;
    public Transform datePoint;
    public Transform line;

    public Transform point;

    public bool createLabels = true;

    private Serializer serializer = new Serializer();
    void Awake()
    {
        _instance = this;
    }

    void Start()
    {
        DateTime epoch = new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc);
        double currTime = Math.Round((double)(DateTime.UtcNow - epoch).TotalMilliseconds);
        Debug.Log("CURRTIME: " + currTime);
        InitializeParticleSystem();
        SetURL("https://graphs2.coinmarketcap.com/currencies/bitcoin/0/" + currTime + "/");
        updateGraph();
    }

    public void kill()
    {
        StopAllCoroutines();
        times = new List<string>();
        prices = new List<double>();
        temp = new List<Particle>();
        serializer = new Serializer();

        if (createLabels)
        {
            foreach (Transform child in transform.parent.Find("dataLabels").transform)
            {
                Destroy(child.gameObject);
            }
        }

        foreach (Transform child in transform.parent.Find("dataLabels").transform)
        {
            if (child.name.Contains("top") || child.name.Contains("min") || child.name.Contains("max"))
            {
                Destroy(child.gameObject);
            }
        }

        pointList = new List<GameObject>();
        builder = new StringBuilder();
    }
    public void updateGraph()
    {
        Async obj = Async.runInCoroutine(GetText);
        obj.onEvent("Scatter", parseData);
    }


    // Extracts the usd price time/price data from the text
    void parseData(object tmp)
    {
        var tournamentList = (List<object>)tmp;

        for (int i = 0; i < tournamentList.Count; i++)
        {
            List<object> curr = tournamentList[i] as List<object>;
            times.Add(curr[0].ToString());
            prices.Add(Convert.ToDouble(curr[1]));

        }

        makeGraph(times, prices);
        //Debug.Log (data);
    }

    // Separates the time and price into separate lists
    void fillData(string allData)
    {
        while (allData.IndexOf("]]") > 5)
        {
            int endIndex = allData.IndexOf(",");
            times.Add(allData.Substring(1, endIndex - 1));

            allData = allData.Substring(endIndex + 2, allData.Length - endIndex - 2);
            endIndex = allData.IndexOf("]");
            prices.Add(Convert.ToDouble(allData.Substring(0, endIndex)));
            allData = allData.Substring(endIndex + 3, allData.Length - endIndex - 3);
        }

        // Debug.Log("Times: " + times.Count + ", Prices: " + prices.Count);
        //  Debug.Log("lastTime: " + times[times.Count - 1] + ", lastPrice: " + prices[prices.Count - 1]);

        makeGraph(times, prices);
    }

    void findStepSize()
    {
        float totalDist = 0;
        for (int i = 0; i < prices.Count - 1; i++)
        {
            float xPos = ((float)i) / (prices.Count / resolution) - 12.5f;
            float xPosNext = ((float)i + 1) / (prices.Count / resolution) - 12.5f;
            float distance = (float)Math.Sqrt(Math.Pow(xPosNext - xPos, 2) + Math.Pow(scaledPrices[i + 1] - scaledPrices[i], 2));

            // Debug.Log("DISTANCE: " + distance);

            totalDist += distance;
        }

        STEP_SIZE = totalDist / (particleCount - prices.Count);
        //  Debug.Log("STEP_SIZE: " + STEP_SIZE);
    }

    void makeGraph(List<string> times, List<double> prices)
    {
        this.times = times;
        this.prices = prices;

        double[] Ys = prices.ToArray();
        rescaleData(Ys, 0);
        findStepSize();

        //Debug.Log("test: " + prices.Count);

        // Creates lines between points, places point prefabs

        float[] Xs = new float[prices.Count];

        //Particle[] particles = new Particle[prices.Count];    

        distances = new Distance[prices.Count - 1];
        additonalParticles = new int[prices.Count];
        Particle tmpParticle;

        //int labelCount = findSpace();
        //Debug.Log("COUNT: " + labelCount);

        for (int i = 0; i < prices.Count - 1; i++)
        {
            float xPos = ((float)i) / (prices.Count / resolution) - 12.5f;
            float xPosNext = ((float)i + 1) / (prices.Count / resolution) - 12.5f;

            float yPos = scaledPrices[i];
            float yPosNext = scaledPrices[i + 1];

            Vector2 position = new Vector2(xPos, yPos);
            Vector2 positionNext = new Vector2(xPosNext, yPosNext);

            float distance = Vector2.Distance(position, positionNext);
            // Debug.Log("INDEX: " + i + ", DISTANCE: " + distance);


            tmpParticle = new Particle();
            tmpParticle.position = new Vector3(xPos, yPos, 0);
            temp.Add(tmpParticle);

            /*             Transform dataPoint = Instantiate(point, new Vector3(0, 0, 0), Quaternion.identity, transform);
                        dataPoint.localPosition = new Vector3(xPos, yPos, 0);
                        dataPoint.name = "topminmax"; */


            // Bottom labels:
            // Only updates when the user selects new time / currency
            int labelCount = (int)Mathf.Round(prices.Count / 15);
            if ((i % labelCount == 0 || i == times.Count - 1) && createLabels)
            {
                Transform currPoint = Instantiate(datePoint, new Vector3(0, 0, 0), Quaternion.identity, transform.parent.Find("dataLabels").transform);
                currPoint.localPosition = new Vector3(xPos, -3f, -2.5f);
                TextMesh dataContent = currPoint.GetComponent<TextMesh>();
                dataContent.text = convertFromTimestamp(times[i]).Month + "/" + convertFromTimestamp(times[i]).Day + "/" + convertFromTimestamp(times[i]).Year;
                dataContent.fontSize = 175;


                Transform currLine = Instantiate(line, new Vector3(0, 0, 0), Quaternion.identity, transform.parent.Find("dataLabels").transform);
                currLine.localPosition = new Vector3(xPos, -2.75f, -2.5f);
                currLine.localScale += new Vector3(0, -0.5f, 0);
                currLine.localRotation = Quaternion.identity;
            }


            labelCount = (int)Mathf.Round(prices.Count / 10);
            // Top labels:
            // Updates always to show displayed timespan
            if ((i % labelCount == 0 || i == times.Count - 1))
            {
                Transform currPoint = Instantiate(datePoint, new Vector3(0, 0, 0), Quaternion.identity, transform.parent.Find("dataLabels").transform);
                currPoint.localPosition = new Vector3(xPos, 3.25f, -2.5f);
                TextMesh dataContent = currPoint.GetComponent<TextMesh>();
                dataContent.text = convertFromTimestamp(times[i]).ToString();
                currPoint.name = "topPoint";
                dataContent.name = "topText";
                dataContent.fontSize = 175;

                Transform currLine = Instantiate(line, new Vector3(0, 0, 0), Quaternion.identity, transform.parent.Find("dataLabels").transform);
                currLine.localPosition = new Vector3(xPos, 2.75f, -2.5f);
                currLine.localScale += new Vector3(0, -0.5f, 0);
                currLine.localRotation = Quaternion.identity;
                currLine.name = "topLine";
            }

            // Adds additional particles between points that are far away
            if (distance > STEP_SIZE)
            {
                float currDist = distance;

                while (currDist > STEP_SIZE)
                {
                    Vector2 norm = (positionNext - position).normalized;

                    position += norm * STEP_SIZE;

                    tmpParticle = new Particle();
                    tmpParticle.position = new Vector3(position.x, position.y, 0);
                    temp.Add(tmpParticle);

                    currDist = Vector2.Distance(position, positionNext);
                }
            }

        }

        // Labels graph with the minimum amount
        Transform minAmount = Instantiate(datePoint, new Vector3(0, 0, 0), Quaternion.identity, transform.parent.Find("dataLabels").transform);
        minAmount.localPosition = new Vector3(-12.35f, scaledPrices[minI] + 0.1f, -2.25f);
        TextMesh minContent = minAmount.GetComponent<TextMesh>();
        minContent.text = "$" + min;
        minAmount.name = "minPoint";
        minContent.name = "minText";
        minContent.fontSize = 200;

        // Labels graph with the maximum amount
        Transform maxAmount = Instantiate(datePoint, new Vector3(0, 0, 0), Quaternion.identity, transform.parent.Find("dataLabels").transform);
        maxAmount.localPosition = new Vector3(-12.35f, scaledPrices[maxI] - 0.05f, -2.25f);
        TextMesh maxContent = maxAmount.GetComponent<TextMesh>();
        maxContent.text = "$" + max;
        maxAmount.name = "maxPoint";
        maxContent.name = "maxText";
        maxContent.fontSize = 200;

        createLabels = false;

        int count = temp.Count;
        tmpParticle = new Particle();
        tmpParticle.position = new Vector3(((float)resolution) - 12.5f, scaledPrices[prices.Count - 1], 0);

        // Fills in all extra particles to the last position
        for (int i = count; i < particleCount; i++)
        {
            temp.Add(tmpParticle);
        }


        dest = temp.ToArray();
        //  Debug.Log("LENGTH: " + dest.Length);

        for (int i = 0; i < dest.Length; i++)
        {
            Vector3 pos = temp[i].position;

            dest[i].position = pos;
            dest[i].color = new Color(Mathf.Pow((pos.x + 10) / 20, 2), Mathf.Pow((pos.y + 10) / 20, 2), Mathf.Pow((pos.z + 10) / 20, 2));

            //Debug.Log("INDEX: " + i + ", DATA: " + dest[i].position.x);
            //}
        }

        currentScale = (maxRange == 0) ? 0 : 10 / maxRange;
        pBuffer.GetData(particles);
        sBuffer.SetData(particles);
        dBuffer.SetData(dest);
        animProgress = 0;
        calculating = false;

    }

    // Rescales the y-values so that they can fit in the graph
    void rescaleData(double[] oldData, int fineTune)
    {
        scaledPrices = new float[oldData.Length];
        max = (float)(oldData[0]);
        min = (float)(oldData[0]);
        minI = 0;
        maxI = 0;
        for (int i = 0; i < oldData.Length; ++i)
        {

            if ((float)(oldData[i]) > max)
            {
                max = (float)(oldData[i]);
                maxI = i;
            }
            if ((float)(oldData[i]) < min)
            {
                min = (float)(oldData[i]);
                minI = i;
            }
        }

        float scale;

    //      Debug.Log("SCALE: " + min / max);
    scalePrices:
        scale = findScale(min, max, fineTune);
        for (int i = 0; i < oldData.Length; ++i)
        {
            scaledPrices[i] = ((float)(oldData[i]) / max * scale) - scale + 2;
            // Temporarily adjusts the scale if the data goes below the graph.
            if (scaledPrices[i] < -2.5)
            {
                fineTune -= 5;
                goto scalePrices;
            }
        }
    }


    // The scale, min/max, represent how close together all of the points are.
    // As scale approaces 0, the points are well dispersed. As scale approaces 1, the points 
    // Are clumped together.
    // The closer to 1 the scale is, the more the points need to be stretched out.
    float findScale(float min, float max, int fineTune)
    {
        //     Debug.Log("Finding Scale");
        if (min / max > 0.9f)
        {
            return min / max * (70 + fineTune);
        }
        else if (min / max > 0.8f)
        {
            return min / max * (35 + fineTune);
        }
        else if (min / max > 0.6f)
        {
            return min / max * (20 + fineTune);
        }
        else if (min / max > 0.4f)
        {
            return min / max * (10 + fineTune);
        }
        else
        {
            return 4;
        }
    }

    IEnumerator GetText(Async routine)
    {
        using (WWW www = new WWW(URL))
        {
            /*             while(!www.isDone){
                            Thread.Sleep(1);
                        } */
            yield return www;
            yield return www.bytes;
            //builder = new StringBuilder();
            //builder.Append(www.text);
            var jsonObj = serializer.Deserialize<object>(www.bytes) as Dictionary<string, object>;
            var tournamentList = jsonObj["price_usd"] as List<object>;
            routine.pushEvent("Scatter", tournamentList);
        }
    }

    void InitializeParticleSystem()
    {
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
    public float effectStrength = 2.0f;

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
        if (particleMaterial != null)
        {
            particleMaterial.SetPass(0);
            particleMaterial.SetVector("_size", particleSize * transform.lossyScale);
            particleMaterial.SetVector("_worldPos", transform.position);

            var m = Matrix4x4.TRS(Vector3.zero, transform.rotation, transform.lossyScale);
            particleMaterial.SetMatrix("_worldRot", m);
            Graphics.DrawProcedural(MeshTopology.Points, particleCount);
        }
    }

    public string getFirstTime()
    {
        //return new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc).AddMilliseconds(Convert.ToDouble(times[0]));
        return this.times[0];
    }

    public string getLastTime()
    {
        //return new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc).AddMilliseconds(Convert.ToDouble(times[times.Count-1]));
        return this.times[this.times.Count - 1];
    }

    public List<string> getTimeList()
    {
        return this.times;
    }

    public DateTime convertFromTimestamp(string timestamp)
    {
        return new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc).AddMilliseconds(Convert.ToDouble(timestamp));
    }

    public void changeChildren()
    {
        foreach (Transform child in transform)
        {
            child.parent = CandleChart.GetInstance().transform;
        }
    }



}