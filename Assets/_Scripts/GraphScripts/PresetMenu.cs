using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class PresetMenu : MonoBehaviour
{

    internal class KeyboardInputResponder : FlexMenu.FlexMenuResponder
    {
        PresetMenu presetMenu;
        internal KeyboardInputResponder(PresetMenu calcInput)
        {
            this.presetMenu = calcInput;
        }

        public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
        {
            presetMenu.HandleInput(sender.name);
        }

        public void Flex_ActionEnd(string name, FlexActionableComponent sender, GameObject collider) { }
    }

    public FlexMenu menu;
    public string defaultFunction = "Astroidal Ellipse";
    CalcManager calcManager;

    [SerializeField]
    private bool cinquefoilKnot, circle, sphereOutline, hypocloid, hypocloidSurface, trefoilKnot,
             turnip, wavySurface, highResSphere;
    [SerializeField]
    private bool astroidalEllipse, bumpySphere, dinisSurface, figure8, graysSurface, knot, mobius,
                 radialWave, torus;
    [SerializeField]
    private bool cone, cube, cylinder, sphere, tetrahedron;

    private Dictionary<string, bool> presets = new Dictionary<string, bool>();
    Scroll scroll;
    JoyStickAggregator joyStickAggregator;

    public void Initialize(CalcManager cm)
    {
        scroll = GetComponentInChildren<Scroll>(true);
        joyStickAggregator = scroll.GetComponent<JoyStickAggregator>();
        calcManager = cm;
        HandleInput(defaultFunction);
        KeyboardInputResponder responder = new KeyboardInputResponder(this);
        menu.RegisterResponder(responder);
        initializePresetButtons();
    }

    //NOTE: make sure actions aren't getting double registered. 
    private void initializePresetButtons()
    {
        #region add to presets
        //R1 -> R1
        presets.Add("Cinquefoil Knot", cinquefoilKnot);
        presets.Add("Circle", circle);
        presets.Add("Sphere Outline", sphereOutline);
        presets.Add("Hypocloid", hypocloid);
        presets.Add("Hypocloid Surface", hypocloidSurface);
        presets.Add("Trefoil Knot", trefoilKnot);
        presets.Add("Turnip", turnip);
        presets.Add("Wavy Surface", wavySurface);
        presets.Add("High-res Sphere", highResSphere);

        //R2 -> R3
        presets.Add("Astroidal Ellipse", astroidalEllipse);
        presets.Add("Bumpy Sphere", bumpySphere);
        presets.Add("Dini's Surface", dinisSurface);
        presets.Add("Figure-8", figure8);
        presets.Add("Gray's Surface", graysSurface);
        presets.Add("Knot", knot);
        presets.Add("Mobius", mobius);
        presets.Add("Radial Wave", radialWave);
        presets.Add("Torus", torus);

        //R3 -> R3
        presets.Add("Cone", cone);
        presets.Add("Cube", cube);
        presets.Add("Cylinder", cylinder);
        presets.Add("Sphere", sphere);
        presets.Add("Tetrahedron", tetrahedron);
        #endregion

        foreach (KeyValuePair<string, bool> pair in presets)
        {
            if (pair.Value == true)
            {
                GameObject presetButton = Instantiate(Resources.Load("Preset", typeof(GameObject))) as GameObject;
                presetButton.GetComponentInChildren<TMPro.TextMeshPro>().text = pair.Key;
                presetButton.name = pair.Key;
                presetButton.SetActive(false);
                scroll.addObject(presetButton.transform);
                scroll.objectParent.GetComponent<FlexPanelComponent>().AddAction(presetButton.GetComponent<FlexActionableComponent>());
                joyStickAggregator.AddForwarder(presetButton.GetComponentInChildren<JoyStickForwarder>());
            }
        }
    }

    protected void HandleInput(string source)
    {
        List<string> x;
        List<string> y;
        List<string> z;
        List<string> umin = ExpressionParser.Parse("");
        List<string> umax = ExpressionParser.Parse("");
        List<string> tmin = ExpressionParser.Parse("");
        List<string> tmax = ExpressionParser.Parse("");
        List<string> vmin = ExpressionParser.Parse("");
        List<string> vmax = ExpressionParser.Parse("");
        List<string> wmin = ExpressionParser.Parse("");
        List<string> wmax = ExpressionParser.Parse("");

        ExpressionSet expressionSet = calcManager.expressionSet;

        switch (source)
        {
            default:
                x = ExpressionParser.Parse("0");
                y = ExpressionParser.Parse("0");
                z = ExpressionParser.Parse("0");
                umin = ExpressionParser.Parse("0");
                umax = ExpressionParser.Parse("0");
                vmin = ExpressionParser.Parse("0");
                vmax = ExpressionParser.Parse("0");
                print("unknown preset pressed");
                break;
            //R1 -> R1
            case "Cinquefoil Knot":
                x = ExpressionParser.Parse("cos(t)*(2-cos(2*t/5))");
                y = ExpressionParser.Parse("sin(t)*(2-cos(2*t/5))");
                z = ExpressionParser.Parse("-sin(2*t/5)");
                tmin = ExpressionParser.Parse("0");
                tmax = ExpressionParser.Parse("10*pi");
                break;
            case "Circle":
                x = ExpressionParser.Parse("cos(t)");
                y = ExpressionParser.Parse("sin(t)");
                z = ExpressionParser.Parse("0");
                tmin = ExpressionParser.Parse("0");
                tmax = ExpressionParser.Parse("2pi");
                break;
            case "Sphere Outline":
                x = ExpressionParser.Parse("sin(t)*cos(32*t)");
                y = ExpressionParser.Parse("sin(t)*sin(32*t)");
                z = ExpressionParser.Parse("cos(t)");
                tmin = ExpressionParser.Parse("0");
                tmax = ExpressionParser.Parse("pi");
                break;
            case "Hypocloid":
                x = ExpressionParser.Parse("cos(t)^3");
                y = ExpressionParser.Parse("sin(t)^3");
                z = ExpressionParser.Parse("0");
                tmin = ExpressionParser.Parse("0");
                tmax = ExpressionParser.Parse("4pi");
                break;
            case "Hypocloid Surface":
                x = ExpressionParser.Parse("10*cos(t*32)^3");
                y = ExpressionParser.Parse("10*sin(t*32)^3");
                z = ExpressionParser.Parse("t-2*pi");
                tmin = ExpressionParser.Parse("0");
                tmax = ExpressionParser.Parse("4pi");
                break;
            case "Trefoil Knot":
                x = ExpressionParser.Parse("sin(t)+2*sin(2*t)");
                y = ExpressionParser.Parse("cos(t)-2*cos(2*t)");
                z = ExpressionParser.Parse("-sin(3*t)");
                tmin = ExpressionParser.Parse("0");
                tmax = ExpressionParser.Parse("2*pi");
                break;
            case "Turnip":
                x = ExpressionParser.Parse("(sin(t/16)^2)*cos(8*t)*8*pi");
                y = ExpressionParser.Parse("(sin(t/16))^2*sin(8*t)*8*pi");
                z = ExpressionParser.Parse("8*pi-t");
                tmin = ExpressionParser.Parse("0");
                tmax = ExpressionParser.Parse("16pi");
                break;
            case "Wavy Surface":
                x = ExpressionParser.Parse("(sin(t/2)^(1/2)*cos(256*t)*pi)");
                y = ExpressionParser.Parse("(sin(t/2))^(1/2)*sin(64*t)*pi");
                z = ExpressionParser.Parse("pi-t");
                tmin = ExpressionParser.Parse("0");
                tmax = ExpressionParser.Parse("2pi");
                break;
            case "High-res Sphere":
                x = ExpressionParser.Parse("sin(t)*cos(1024*t)");
                y = ExpressionParser.Parse("sin(t)*sin(1024*t)");
                z = ExpressionParser.Parse("cos(t)");
                tmin = ExpressionParser.Parse("0");
                tmax = ExpressionParser.Parse("pi");
                break;
            //R2->R3
            case "Astroidal Ellipse":
                x = ExpressionParser.Parse("(2*cos(u)*cos(v))^3");
                y = ExpressionParser.Parse("(2*sin(u)*cos(v))^3");
                z = ExpressionParser.Parse("(2*sin(v))^3");
                umin = ExpressionParser.Parse("0");
                umax = ExpressionParser.Parse("pi");
                vmin = ExpressionParser.Parse("0");
                vmax = ExpressionParser.Parse("2*pi");
                break;
            case "Bumpy Sphere":
                x = ExpressionParser.Parse("5*sin(u)*sin(v)+cos(30*v)*0.15");
                y = ExpressionParser.Parse("5*cos(u)*sin(v)+cos(30*u)*0.15");
                z = ExpressionParser.Parse("5*cos(v)");
                umin = ExpressionParser.Parse("0");
                umax = ExpressionParser.Parse("2*pi");
                vmin = ExpressionParser.Parse("0");
                vmax = ExpressionParser.Parse("pi");
                break;
            case "Dini's Surface":
                x = ExpressionParser.Parse("3*cos(u)*sin(v)");
                y = ExpressionParser.Parse("3*sin(u)*sin(v)");
                z = ExpressionParser.Parse("3*(cos(v)+log(tan(0.5*v)))+0.4*u");
                umin = ExpressionParser.Parse("-4*pi");
                umax = ExpressionParser.Parse("4*pi");
                vmin = ExpressionParser.Parse("0.005");
                vmax = ExpressionParser.Parse("3.135");
                break;
            case "Figure-8":
                x = ExpressionParser.Parse("(3+cos(u/2)*sin(v)-sin(u/2)*sin(2*v))*cos(u)");
                y = ExpressionParser.Parse("(3+cos(u/2)*sin(v)-sin(u/2)*sin(2*v))*sin(u)");
                z = ExpressionParser.Parse("sin(u/2)*sin(v)-cos(u/2)*sin(2*v)");
                umin = ExpressionParser.Parse("0");
                umax = ExpressionParser.Parse("2*pi");
                vmin = ExpressionParser.Parse("0");
                vmax = ExpressionParser.Parse("2*pi");
                break;
            case "Gray's Surface":
                x = ExpressionParser.Parse("(3+cos(3*u/2)*sin(v)-sin(3*u/2)*sin(2*v))*cos(u/2)");
                y = ExpressionParser.Parse("(3+cos(3*u/2)*sin(v)-sin(3*u/2)*sin(2*v))*sin(u/2)");
                z = ExpressionParser.Parse("sin(3*u/2)*sin(v)+cos(3*u/2)*sin(2*v)");
                umin = ExpressionParser.Parse("0");
                umax = ExpressionParser.Parse("4*pi");
                vmin = ExpressionParser.Parse("0");
                vmax = ExpressionParser.Parse("2pi");
                break;
            case "Knot":
                x = ExpressionParser.Parse("3*sin(3*u)/2+cos(v)))");
                y = ExpressionParser.Parse("3*(sin(u)+2*sin(2*u))/(2+cos(v+pi*2/3))");
                z = ExpressionParser.Parse("3/2*(cos(u)-2*cos(2*u))*(2+cos(v))*(2+cos(v+pi*2/3))/4");
                umin = ExpressionParser.Parse("-pi");
                umax = ExpressionParser.Parse("2pi");
                vmin = ExpressionParser.Parse("-pi");
                vmax = ExpressionParser.Parse("2pi");
                break;
            case "Mobius":
                x = ExpressionParser.Parse("(5+u*cos(0.5*v))*cos(v)");
                y = ExpressionParser.Parse("(5+u*cos(0.5*v))*sin(v)");
                z = ExpressionParser.Parse("u*sin(0.5*v)");
                umin = ExpressionParser.Parse("-1");
                umax = ExpressionParser.Parse("1");
                vmin = ExpressionParser.Parse("0");
                vmax = ExpressionParser.Parse("2*pi");
                break;
            case "Radial Wave":
                x = ExpressionParser.Parse("u");
                y = ExpressionParser.Parse("v");
                z = ExpressionParser.Parse("cos((u^2+v^2)^0.5)");
                umin = ExpressionParser.Parse("-10");
                umax = ExpressionParser.Parse("10");
                vmin = ExpressionParser.Parse("-10");
                vmax = ExpressionParser.Parse("10");
                break;
            case "Torus":
                x = ExpressionParser.Parse("(5+cos(v))*cos(u)");
                y = ExpressionParser.Parse("(5+cos(v))*sin(u)");
                z = ExpressionParser.Parse("sin(v)");
                umin = ExpressionParser.Parse("0");
                umax = ExpressionParser.Parse("2pi");
                vmin = ExpressionParser.Parse("0");
                vmax = ExpressionParser.Parse("2pi");
                break;
            //R3->R3
            case "Cone":
                x = ExpressionParser.Parse("ucos(v)w");
                y = ExpressionParser.Parse("usin(v)w");
                z = ExpressionParser.Parse("w");
                umin = ExpressionParser.Parse("0");
                umax = ExpressionParser.Parse("1");
                vmin = ExpressionParser.Parse("0");
                vmax = ExpressionParser.Parse("2pi");
                wmin = ExpressionParser.Parse("0");
                wmax = ExpressionParser.Parse("1");
                break;
            case "Cube":
                x = ExpressionParser.Parse("u");
                y = ExpressionParser.Parse("v");
                z = ExpressionParser.Parse("w");
                umin = ExpressionParser.Parse("0");
                umax = ExpressionParser.Parse("1");
                vmin = ExpressionParser.Parse("0");
                vmax = ExpressionParser.Parse("1");
                wmin = ExpressionParser.Parse("0");
                wmax = ExpressionParser.Parse("1");
                break;
            case "Cylinder":
                x = ExpressionParser.Parse("ucos(v)");
                y = ExpressionParser.Parse("usin(v)");
                z = ExpressionParser.Parse("w");
                umin = ExpressionParser.Parse("0");
                umax = ExpressionParser.Parse("1");
                vmin = ExpressionParser.Parse("0");
                vmax = ExpressionParser.Parse("2pi");
                wmin = ExpressionParser.Parse("0");
                wmax = ExpressionParser.Parse("1");
                break;
            case "Sphere":
                x = ExpressionParser.Parse("ucos(v)sin(w)");
                y = ExpressionParser.Parse("usin(v)sin(w)");
                z = ExpressionParser.Parse("ucos(w)");
                umin = ExpressionParser.Parse("0");
                umax = ExpressionParser.Parse("1");
                vmin = ExpressionParser.Parse("0");
                vmax = ExpressionParser.Parse("2pi");
                wmin = ExpressionParser.Parse("0");
                wmax = ExpressionParser.Parse("2pi");
                break;
            case "Tetrahedron":
                x = ExpressionParser.Parse("u(1-v)");
                y = ExpressionParser.Parse("uv(1-w)");
                z = ExpressionParser.Parse("uvw");
                umin = ExpressionParser.Parse("0");
                umax = ExpressionParser.Parse("1");
                vmin = ExpressionParser.Parse("0");
                vmax = ExpressionParser.Parse("1");
                wmin = ExpressionParser.Parse("0");
                wmax = ExpressionParser.Parse("1");
                break;
        }

        expressionSet.AddExpression("X", x);
        expressionSet.AddExpression("Y", y);
        expressionSet.AddExpression("Z", z);
        expressionSet.AddRange("t", tmin, tmax);
        expressionSet.AddRange("u", umin, umax);
        expressionSet.AddRange("v", vmin, vmax);
        expressionSet.AddRange("w", wmin, wmax);
        calcManager.PresetPressed();
    }
}
