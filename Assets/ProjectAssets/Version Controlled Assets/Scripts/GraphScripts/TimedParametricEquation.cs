using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using AK;

/// <summary>
/// class for parametric visualization that supports time lapse
/// </summary>
public class TimedParametricEquation : MonoBehaviour {

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

    private ExpressionSolver solver;
    

	// Use this for initialization
	void Start () {
        InitializeParticleSystem();
	}
	
	// Update is called once per frame
	void Update () {
		
	}

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

}
