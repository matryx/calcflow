using UnityEngine;
using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Threading;

public class AnimatedParticleGrapher : MonoBehaviour
{
    public static AnimatedParticleGrapher _instance;

     Particle[] particles;
     Particle[] source;
     Particle[] dest;

     float animProgress;

    public float particleSize = 0.05f;
    public Texture2D particleSprite;
    public Shader particleShader;
    public ComputeShader particleAnimation;

     Material particleMaterial;
     ComputeBuffer pBuffer;
     ComputeBuffer sBuffer;
     ComputeBuffer dBuffer;

    // size of a particle struct in bytes
     const int PARTICLE_SIZE = 2 * 12 + 16;
    // number of threads for a group
     const int GROUP_SIZE = 256;

     int threadGroups;

    public enum ParticleEffectList
    {
        None, Gravity, Lerp,
        SmoothLerp, Explode,
        Swirl
    }
     string[] kernels =
    {
        "None", "Gravity", "Lerp",
        "SmoothLerp", "Explode",
        "Swirl"
    };
    public ParticleEffectList particleEffect = ParticleEffectList.None;

    [Range(0.1f, 2.0f)]
    public float effectStrength = 2.0f;

    int particleCount;
    public Gradient gradient;

    protected void Awake()
    {
        _instance = this;
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
        initialized = true;
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


    ///<summary>
    ///Changes particle graph and regraphs.
    ///</summary>
    // Reinitializes particles system for the new count.
    // If you do not want to reinitialize the particle system, 
    // make sure that the number of particles used by your function is the same for each run.
    public void ChangeParticleCount(int count)
    {
        particleCount = count;
        InitializeParticleSystem();
    }

     void MoveParticles()
    {
        pBuffer.GetData(particles);
        sBuffer.SetData(particles);
        dBuffer.SetData(dest);
        animProgress = 0;
    }

    public bool isGraphing()
    {
        return !(animProgress == 1);
    }

    bool initialized = false;

    public void PlotParticles(Particle[] particles)
    {
        Debug.Log("plottingPoints");
        animProgress = 0;

        if (particles.Length != particleCount)
        {
            Debug.LogError("Number of particles not equal to number of passed points");
            Debug.LogError("Particles: " + particleCount + " Points: " + particles.Length);
            return;
        }
        if (!initialized)
        {
            InitializeParticleSystem();
        }
        dest = particles;
        MoveParticles();
    }
}