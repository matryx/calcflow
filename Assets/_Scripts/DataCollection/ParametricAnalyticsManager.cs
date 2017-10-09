using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Analytics;

public class ParametricAnalyticsManager : MonoBehaviour
{

    private int astroidal, bumpy, dini, knot, figure8,
                torus, mobius, radial, trefoil, cinquefoil;

    // Use this for initialization
    void Start()
    {
        astroidal = 0;
        bumpy = 0;
        dini = 0;
        knot = 0;
        figure8 = 0;
        torus = 0;
        mobius = 0;
        radial = 0;
        trefoil = 0;
        cinquefoil = 0;
    }

    public void IncrementPreset(string preset)
    {
        switch (preset.ToLower())
        {
            case "astroidal":
                astroidal++;
                break;
            case "bumpy":
                bumpy++;
                break;
            case "dini":
                dini++;
                break;
            case "knot":
                knot++;
                break;
            case "figure8":
                figure8++;
                break;
            case "torus":
                torus++;
                break;
            case "mobius":
                mobius++;
                break;
            case "radial":
                radial++;
                break;
            case "trefoil":
                trefoil++;
                break;
            case "cinquefoil":
                cinquefoil++;
                break;
            default:
                break;
        }
    }

    void CreateCustomEvent()
    {
        Analytics.CustomEvent("Parametric Presets Count", new System.Collections.Generic.Dictionary<string, object> {
            { "Astroidal", astroidal },
            { "Bumpy", bumpy },
            { "Dini", dini },
            { "Knot", knot },
            { "Figure8", figure8},
            { "Torus", torus },
            { "Mobius", mobius},
            { "Radial", radial},
            { "Trefoil", trefoil},
            { "Cinquefoil", cinquefoil }
        });
    }

    void OnDestroy()
    {
        CreateCustomEvent();
    }

    void OnApplicationQuit()
    {
        CreateCustomEvent();
    }
    // Update is called once per frame
    void Update()
    {

    }
}
