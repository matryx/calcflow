using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ParticleCountMenu : MonoBehaviour
{
    internal class KeyboardInputResponder : FlexMenu.FlexMenuResponder
    {
        ParticleCountMenu particleCountMenu;
        internal KeyboardInputResponder(ParticleCountMenu particleCountMenu)
        {
            this.particleCountMenu = particleCountMenu;
        }

        public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
        {
            particleCountMenu.HandleInput(sender.name);
        }

        public void Flex_ActionEnd(string name, FlexActionableComponent sender, GameObject collider) { }

    }

    public int low = 40000;
    public int medium = 10000;
    public int high = 262144;
    public int ultra = 360000;

    public void HandleInput(string buttonID)
    {
        #region switch
        switch (buttonID)
        {
            default:
                break;
            case "Low":
                paramSurface.ChangeParticleCount(low);
                break;
            case "Medium":
                paramSurface.ChangeParticleCount(medium);
                break;
            case "High":
                paramSurface.ChangeParticleCount(high);
                break;
            case "Ultra":
                paramSurface.ChangeParticleCount(ultra);
                break;
        }
        #endregion

        // @stats
        // particle setting
        Calcflow.UserStatistics.StatisticsTracking.InstantEvent("Button Click", "Particle Setting",
        new Dictionary<string, object>()
        {
            {"buttonName", buttonID}
        });
    }

    CustomParametrizedSurface paramSurface;

    public void Awake()
    {
        FlexMenu keyboard = GetComponent<FlexMenu>();
        KeyboardInputResponder responder = new KeyboardInputResponder(this);
        keyboard.RegisterResponder(responder);

        paramSurface = FindObjectOfType<CustomParametrizedSurface>();

    }
}
