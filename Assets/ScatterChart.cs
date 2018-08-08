using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System;
using System.Text;
using UnityEngine.Networking;
using Nanome.Core;

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
    private int threadGroups;
    private Material particleMaterial;
    private ComputeBuffer pBuffer;
    private ComputeBuffer sBuffer;
    private ComputeBuffer dBuffer;
    public float particleSize = 0.05f;
    public Texture2D particleSprite;
    public Shader particleShader;
    public ComputeShader particleAnimation;
    private const int PARTICLE_SIZE = 2 * 12 + 16;
    private const int GROUP_SIZE = 256;
    private float animProgress;
    public float currentScale = 10;
    public ParticleEffectList particleEffect = ParticleEffectList.None;
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

    int particleCount;
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
        URL = newURL;
    }

    // Use this for initialization
    string output;
    StringBuilder builder = new StringBuilder();
    public List<string> times = new List<string>();
    public List<string> prices = new List<string>();

    public List<GameObject> pointList = new List<GameObject>();
    float[] scaledPrices;

    GameObject frameObj;
    LineRenderer frameLine;

    // The factor by which number of points are determined. 1 = full points = lag
    public int numPoints = 3;

    // Higher resolution will stretch out the graph, lower will compress it.
    float resolution = 25f;
    //public int numPoints = 10;
    public Material frameMaterial;
    public Transform point;

    void Awake()
    {
        _instance = this;
    }

    void Start()
    {
        DateTime epoch = new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc);
        double currTime = Math.Round((double)(DateTime.UtcNow - epoch).TotalMilliseconds);
        Debug.Log("CURRTIME: " + currTime);
        SetURL("https://graphs2.coinmarketcap.com/currencies/bitcoin/0/" + currTime + "/");
        updateGraph();
    }

    public void kill()
    {
        StopAllCoroutines();
        Destroy(frameLine);
        times = new List<string>();
        prices = new List<string>();

        foreach (Transform child in transform)
        {
            Destroy(child.gameObject);
        }

        pointList = new List<GameObject>();
        builder = new StringBuilder();
    }
    public void updateGraph()
    {
        Async obj = Async.runInCoroutine(GetText);
        obj.onEvent("Done", parseData);
    }

    // Extracts the usd price time/price data from the text
    void parseData(object tmp)
    {
        Debug.Log("parseData");
        string data = ((StringBuilder)tmp).ToString();
        int startIndex = data.IndexOf("price_usd");
        int endIndex = data.IndexOf("]]", startIndex);

        //Debug.Log ("start: " + startIndex + ", End: " + endIndex);
        data = data.Substring(startIndex + 13, endIndex - startIndex);
        fillData(data);
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
            prices.Add(allData.Substring(0, endIndex));
            allData = allData.Substring(endIndex + 3, allData.Length - endIndex - 3);
        }

        Debug.Log("Times: " + times.Count + ", Prices: " + prices.Count);
        Debug.Log("lastTime: " + times[times.Count - 1] + ", lastPrice: " + prices[prices.Count - 1]);

        makeGraph(times, prices);
    }

    void makeGraph(List<string> times, List<string> prices)
    {
        this.times = times;
        this.prices = prices;

        particleCount = prices.Count;

        string[] Ys = prices.ToArray();
        rescaleData(Ys, 0);

        //Debug.Log("test: " + prices.Count);

        // Creates lines between points, places point prefabs

        float[] Xs = new float[prices.Count];

        //Particle[] particles = new Particle[prices.Count];



        distances = new Distance[prices.Count - 1];
        additonalParticles = new int[prices.Count];

        for (int i = 0; i < prices.Count - 1; i++)
        {
            float xPos = ((float)i) / (prices.Count / resolution) - 12.5f;
            float xPosNext = ((float)i + 1) / (prices.Count / resolution) - 12.5f;

            //Debug.Log("POINT: " + i + ", DISTANCE: " + Math.Sqrt(Math.Pow(temp[i+1].position.x-temp[i].position.x, 2) + Math.Pow(temp[i+1].position.y-temp[i].position.y, 2)));
            distances[i].distance = (float)Math.Sqrt(Math.Pow(xPosNext - xPos, 2) + Math.Pow(scaledPrices[i + 1] - scaledPrices[i], 2));
            distances[i].point1 = new Particle();
            distances[i].point1.position = new Vector3(xPos, scaledPrices[i]);

            distances[i].point2 = new Particle();
            distances[i].point2.position = new Vector3(xPosNext, scaledPrices[i + 1]);

            float distance = 1000f * distances[i].distance;
            additonalParticles[i] = (int)Mathf.Floor(distance);

            particleCount += additonalParticles[i];

        }

        Particle[] temp = new Particle[particleCount];

        int fillIndex = 0;
        Debug.Log("COUNT: " + particleCount);

        for (int i = 0; i < particleCount;)
        {
            //Ys [i] = Random.Range (-2f, 2f);
            float xPos = ((float)i) / (particleCount / resolution) - 12.5f;
            temp[i] = new Particle();
            temp[i].position = new Vector3(xPos, scaledPrices[fillIndex], 0);

            Debug.Log("INDEX: " + i + ", FILLINDEX: " + fillIndex);

            i += additonalParticles[fillIndex]+1;
            fillIndex++;
            //pointList.Add(currPoint);
        }

        InitializeParticleSystem();

        dest = temp;
        int space = 0;
        bool isGap = false;
        Debug.Log("LENGTH: " + dest.Length);

        for (int i = 0; i < dest.Length; i++)
        {
            Vector3 pos = temp[i].position;
            if (space > 1900)
                Debug.Log("INDEX: " + i + ", SPACE: " + space);
            if (temp[i].position == Vector3.zero)
            {
                isGap = true;

                float distance = distances[space].distance;
                float numPoints = additonalParticles[space];

                float xDist = (distances[space].point2.position.x - distances[space].point1.position.x) / numPoints;
                float yDist = (distances[space].point2.position.y - distances[space].point1.position.y) / numPoints;

                for (int j = 0; j < numPoints; j++)
                {
                    pos = new Vector3(xDist * j, yDist * j, 0);
                    dest[i + j].position = pos;
                    dest[i + j].color = new Color(Mathf.Pow((pos.x + 10) / 20, 2), Mathf.Pow((pos.y + 10) / 20, 2), Mathf.Pow((pos.z + 10) / 20, 2));
                }
            }
            else
            {
                if (isGap) space++;

                isGap = false;
                dest[i].position = pos;
                dest[i].color = new Color(Mathf.Pow((pos.x + 10) / 20, 2), Mathf.Pow((pos.y + 10) / 20, 2), Mathf.Pow((pos.z + 10) / 20, 2));
            }
        }

        currentScale = (maxRange == 0) ? 0 : 10 / maxRange;
        pBuffer.GetData(particles);
        sBuffer.SetData(particles);
        dBuffer.SetData(dest);
        animProgress = 0;
        calculating = false;

    }

    // Rescales the y-values to fit between [-2,2] so that it can fit in the graph
    void rescaleData(string[] oldData, int fineTune)
    {
        scaledPrices = new float[oldData.Length];
        float max = float.Parse(oldData[0]);
        float min = float.Parse(oldData[0]);
        int minI = 0;
        for (int i = 0; i < oldData.Length; ++i)
        {

            if (float.Parse(oldData[i]) > max)
            {
                max = float.Parse(oldData[i]);
            }
            if (float.Parse(oldData[i]) < min)
            {
                min = float.Parse(oldData[i]);
                minI = i;
            }
        }

        float scale;

        Debug.Log("SCALE: " + min / max);
    scalePrices:
        scale = findScale(min, max, fineTune);
        for (int i = 0; i < oldData.Length; ++i)
        {
            scaledPrices[i] = (float.Parse(oldData[i]) / max * scale) - scale + 2;
            // Temporarily adjusts the scale if the data goes below the graph.
            if (scaledPrices[i] < -2.5)
            {
                fineTune -= 5;
                goto scalePrices;
            }
        }
    }

    // The scale, min/max, represent how close together all of the points are.
    // As scale approaces 0, the points are well disbursed. As scale approaces 1, the points 
    // Are clumped together.
    // The closer to 1 the scale is, the more the points need to be stretched out.
    float findScale(float min, float max, int fineTune)
    {
        Debug.Log("Finding Scale");
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
            yield return www;
            yield return www.text;
            builder.Append(www.text);
            routine.pushEvent("Done", builder);
        }
    }

    void InitializeParticleSystem()
    {
        threadGroups = Mathf.CeilToInt((float)particleCount / GROUP_SIZE);
        int l = prices.Count;

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
            Graphics.DrawProcedural(MeshTopology.Points, prices.Count);
        }
    }

}