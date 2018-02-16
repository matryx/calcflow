using UnityEngine;
using System.Collections;
using System.Collections.Generic;
using VoxelBusters.RuntimeSerialization;

[RuntimeSerializable(typeof(MonoBehaviour), true, true)]
internal class ParticleSettingsResponder : FlexMenu.FlexMenuResponder
{
    internal bool isReady = false;
    internal CustomParametrizedSurface paramSurface;

    [HideInInspector] public FlexActionableComponent effect;
    [HideInInspector] public FlexActionableComponent speed;

    internal ParticleSettingsResponder(CustomParametrizedSurface psurf)
    {
        paramSurface = psurf;
    }

    public void initialize(FlexActionableComponent defaultEffect, FlexActionableComponent defaultSpeed)
    {
        effect = defaultEffect;
        speed = defaultSpeed;
        Flex_ActionStart(defaultEffect.name, defaultEffect, null);
    }

    public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
    {
        switch (sender.name)
        {
            case "None":
                paramSurface.particleEffect = CustomParametrizedSurface.ParticleEffectList.None;
                if (effect != null) effect.SetState(0);
                effect = sender;
                break;
            case "Gravity":
                paramSurface.particleEffect = CustomParametrizedSurface.ParticleEffectList.Gravity;
                if (effect != null) effect.SetState(0);
                effect = sender;
                break;
            case "Lerp":
                paramSurface.particleEffect = CustomParametrizedSurface.ParticleEffectList.Lerp;
                if (effect != null) effect.SetState(0);
                effect = sender;
                break;
            case "SmoothLerp":
                paramSurface.particleEffect = CustomParametrizedSurface.ParticleEffectList.SmoothLerp;
                if (effect != null) effect.SetState(0);
                effect = sender;
                break;
            case "Explode":
                paramSurface.particleEffect = CustomParametrizedSurface.ParticleEffectList.Explode;
                if (effect != null) effect.SetState(0);
                effect = sender;
                break;
            case "Swirl":
                paramSurface.particleEffect = CustomParametrizedSurface.ParticleEffectList.Swirl;
                if (effect != null) effect.SetState(0);
                effect = sender;
                break;
            case "SpeedFast":
                paramSurface.effectStrength = 2f;
                if (speed != null) speed.SetState(0);
                speed = sender;
                break;
            case "SpeedMed":
                paramSurface.effectStrength = 1f;
                if (speed != null) speed.SetState(0);
                speed = sender;
                break;
            case "SpeedSlow":
                paramSurface.effectStrength = 0.5f;
                if (speed != null) speed.SetState(0);
                speed = sender;
                break;
        }
        isReady = true;
        if (effect != null) effect.SetState(2);
        if (speed != null) speed.SetState(2);
    }

    public void Flex_ActionEnd(string name, FlexActionableComponent sender, GameObject collider)
    {
    }
}

public class ParticleAnimationSettings : MonoBehaviour
{

    FlexMenu settings;
    ParticleSettingsResponder responder;
    FlexActionableComponent defaultSpeed;
    FlexActionableComponent defaultEffect;
    CustomParametrizedSurface paramSurface;

    // Use this for initialization
    public void Initialize(CalcManager calcManager)
    {
        paramSurface = calcManager.paramSurface;
        defaultSpeed = calcManager.defaultSpeed;
        defaultEffect = calcManager.defaultEffect;

        responder = new ParticleSettingsResponder(paramSurface);
        GetComponent<FlexMenu>().RegisterResponder(responder);
        responder.initialize(defaultEffect, defaultSpeed);
    }

    // Update is called once per frame
    void Update()
    {
        if (responder.isReady)
        {
            responder.isReady = false;
        }
    }
}


