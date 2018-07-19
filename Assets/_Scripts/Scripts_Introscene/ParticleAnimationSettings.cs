using UnityEngine;
using System.Collections;
using System.Collections.Generic;

internal class ParticleSettingsResponder : FlexMenu.FlexMenuResponder
{
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
        if (effect != null) effect.SetState(2);
        if (speed != null) speed.SetState(2);
    }

    public void Flex_ActionEnd(string name, FlexActionableComponent sender, GameObject collider)
    {
    }
}

public class ParticleAnimationSettings : MonoBehaviour
{

    private class ParticleSettingsResponder : FlexMenu.FlexMenuResponder
    {
        private ParticleAnimationSettings particleSettings;


        internal ParticleSettingsResponder(ParticleAnimationSettings particleSettings)
        {
            this.particleSettings = particleSettings;
        }

        public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
        {
            particleSettings.HandleInput(sender);
        }

        public void Flex_ActionEnd(string name, FlexActionableComponent sender, GameObject collider)
        {
        }
    }

    private FlexActionableComponent effect;
    private FlexActionableComponent speed;
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

        effect = defaultEffect;
        speed = defaultSpeed;

        responder = new ParticleSettingsResponder(this);
        GetComponent<FlexMenu>().RegisterResponder(responder);
        HandleInput(defaultEffect);
    }

    public void HandleInput(FlexActionableComponent sender)
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
        if (effect != null) effect.SetState(2);
        if (speed != null) speed.SetState(2);
    }

}


