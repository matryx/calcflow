using System.Collections;
using System.Collections.Generic;
using UnityEngine;

// THIS FILE WILL BE DELETED, IT IS MERELY FOR QUICK BUILDING PURPOSES
public class DensityPresetMenu : MonoBehaviour
{

    internal class KeyboardInputResponder : FlexMenu.FlexMenuResponder
    {
        DensityPresetMenu densityPresetMenu;
        internal KeyboardInputResponder(DensityPresetMenu calcInput)
        {
            this.densityPresetMenu = calcInput;
        }

        public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
        {
            densityPresetMenu.HandleInput(sender.name);
        }

        public void Flex_ActionEnd(string name, FlexActionableComponent sender, GameObject collider) { }
    }

    public FlexMenu menu;
    public string defaultFunction = "3P";
    DensityInputManager densManager;

    [SerializeField]
    private bool s1, s2, p2, p3, d3z2, d3xy, p4, px, py, pz, dz2, dxz, dyz, dxy, dx2y2, fz3, fxz2, fyz2, fxyz, fzx2y2, fxx23y2, fy3x2y2;

    private Dictionary<string, bool> presets = new Dictionary<string, bool>();
    Scroll scroll;
    JoyStickAggregator joyStickAggregator;

    public void Initialize(DensityInputManager dm)
    {
        scroll = GetComponentInChildren<Scroll>(true);
        joyStickAggregator = scroll.GetComponent<JoyStickAggregator>();
        densManager = dm;
        HandleInput(defaultFunction);
        KeyboardInputResponder responder = new KeyboardInputResponder(this);
        menu.RegisterResponder(responder);
        initializePresetButtons();
    }

    //NOTE: make sure actions aren't getting double registered. 
    private void initializePresetButtons()
    {
        #region add to presets

        //Oribals
        presets.Add("1S", s1);
        presets.Add("2S", s2);        
        presets.Add("2Pz", p2);
        presets.Add("3Pz", p3);
        presets.Add("3Dz\x00B2", d3z2);
        presets.Add("3Dxy", d3xy);
        presets.Add("4Pz", p4);
        presets.Add("Px", px);
        presets.Add("Py", py);
        presets.Add("Pz", pz);
        presets.Add("Dz\x00B2", dz2);
        presets.Add("Dxz", dxz);
        presets.Add("Dyz", dyz);
        presets.Add("Dxy", dxy);
        presets.Add("Dx\x00B2-y\x00B2", dx2y2);
        presets.Add("Fz\x00B3", fz3);
        presets.Add("Fxz\x00B2", fxz2);
        presets.Add("Fyz\x00B2", fyz2);
        presets.Add("Fxyz", fxyz);
        presets.Add("Fz\x0028x\x00B2-y\x00B2\x0029", fzx2y2);
        presets.Add("Fx\x0028x\x00B2-3y\x00B2\x0029", fxx23y2);
        presets.Add("Fy\x00283x\x00B2-y\x00B2\x0029", fy3x2y2);
        #endregion

        foreach (KeyValuePair<string, bool> pair in presets)
        {
            if (pair.Value == true)
            {
                GameObject presetButton = Instantiate(Resources.Load("DensityPreset", typeof(GameObject))) as GameObject;
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

        ExpressionSet expressionSet = densManager.es;

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
            // Orbitals
            case "1S":
                x = ExpressionParser.Parse("(1.772454*e^(-1*(x^2+y^2+z^2)^(1/2)))^2");
                y = ExpressionParser.Parse("0");
                z = ExpressionParser.Parse("0");
                densManager.setRange(3);
                break;
            case "2S":
                x = ExpressionParser.Parse("(0.099735*(2-(x^2+y^2+z^2)^(1/2))*e^(-1*((x^2+y^2+z^2)^(1/2))/2))^2");
                y = ExpressionParser.Parse("0");
                z = ExpressionParser.Parse("0");
                densManager.setRange(10);
                break;
            case "2Pz":
                x = ExpressionParser.Parse("(0.099735*z*e^(-1*((x^2+y^2+z^2)^(1/2))/2))^2");
                y = ExpressionParser.Parse("0");
                z = ExpressionParser.Parse("0");
                densManager.setRange(12);
                break;
            case "3Pz":
                x = ExpressionParser.Parse("(0.004925*(6-(x^2+y^2+z^2)^(1/2))*z*e^(-1*((x^2+y^2+z^2)^(1/2))/3))^2");
                y = ExpressionParser.Parse("0");
                z = ExpressionParser.Parse("0");
                densManager.setRange(22);
                break;
            case "3Dz\x00B2":
                goto case "3Dz2";
            case "3Dz2":
                x = ExpressionParser.Parse("(0.002844*(3*z^2-(x^2+y^2+z^2))*e^(-1*((x^2+y^2+z^2)^(1/2))/3))^2");
                y = ExpressionParser.Parse("0");
                z = ExpressionParser.Parse("0");
                densManager.setRange(24);
                break;
            case "3Dxy":
                x = ExpressionParser.Parse("(0.009850*x*y*e^(-1*((x^2+y^2+z^2)^(1/2))/3))^2");
                y = ExpressionParser.Parse("0");
                z = ExpressionParser.Parse("0");
                densManager.setRange(24);
                break;
            case "4Pz":
                x = ExpressionParser.Parse("(0.039424*(1-1/4*(x^2+y^2+z^2)^(1/2)+1/80*(x^2+y^2+z^2))*z*e^(-1*((x^2+y^2+z^2)^(1/2))/4))^2");
                y = ExpressionParser.Parse("0");
                z = ExpressionParser.Parse("0");
                densManager.setRange(34);
                break;
            case "Px":
                x = ExpressionParser.Parse("(3^(1/2)/2*sin(u)*1/pi^(1/2)*sin(v))^2*sin(u)*cos(v)");
                y = ExpressionParser.Parse("(3^(1/2)/2*sin(u)*1/pi^(1/2)*sin(v))^2*sin(u)*sin(v)");
                z = ExpressionParser.Parse("(3^(1/2)/2*sin(u)*1/pi^(1/2)*sin(v))^2*cos(u)");
                umin = ExpressionParser.Parse("0");
                umax = ExpressionParser.Parse("pi");
                vmin = ExpressionParser.Parse("0");
                vmax = ExpressionParser.Parse("2pi");
                break;
            case "Py":
                x = ExpressionParser.Parse("(3^(1/2)/2*sin(u)*1/pi^(1/2)*cos(v))^2*sin(u)*cos(v)");
                y = ExpressionParser.Parse("(3^(1/2)/2*sin(u)*1/pi^(1/2)*cos(v))^2*sin(u)*sin(v)");
                z = ExpressionParser.Parse("(3^(1/2)/2*sin(u)*1/pi^(1/2)*cos(v))^2*cos(u)");
                umin = ExpressionParser.Parse("0");
                umax = ExpressionParser.Parse("pi");
                vmin = ExpressionParser.Parse("0");
                vmax = ExpressionParser.Parse("2pi");
                break;
            case "Pz":
                x = ExpressionParser.Parse("(3^(1/2)/2*cos(u)*1/pi^(1/2))^2*sin(u)*cos(v)");
                y = ExpressionParser.Parse("(3^(1/2)/2*cos(u)*1/pi^(1/2))^2*sin(u)*sin(v)");
                z = ExpressionParser.Parse("(3^(1/2)/2*cos(u)*1/pi^(1/2))^2*cos(u)");
                umin = ExpressionParser.Parse("0");
                umax = ExpressionParser.Parse("pi");
                vmin = ExpressionParser.Parse("0");
                vmax = ExpressionParser.Parse("2pi");
                break;
            case "Dz\x00B2":
                goto case "Dz2";
            case "Dz2":
                x = ExpressionParser.Parse("(5^(1/2)/4*(3*((cos(u))^2)-1)*1/pi^(1/2))^2*sin(u)*cos(v)");
                y = ExpressionParser.Parse("(5^(1/2)/4*(3*((cos(u))^2)-1)*1/pi^(1/2))^2*sin(u)*sin(v)");
                z = ExpressionParser.Parse("(5^(1/2)/4*(3*((cos(u))^2)-1)*1/pi^(1/2))^2*cos(u)");
                umin = ExpressionParser.Parse("0");
                umax = ExpressionParser.Parse("pi");
                vmin = ExpressionParser.Parse("0");
                vmax = ExpressionParser.Parse("2pi");
                break;
            case "Dxz":
                x = ExpressionParser.Parse("(15^(1/2)/2*sin(u)*cos(u)*1/pi^(1/2)*sin(v))^2*sin(u)*cos(v)");
                y = ExpressionParser.Parse("(15^(1/2)/2*sin(u)*cos(u)*1/pi^(1/2)*sin(v))^2*sin(u)*sin(v)");
                z = ExpressionParser.Parse("(15^(1/2)/2*sin(u)*cos(u)*1/pi^(1/2)*sin(v))^2*cos(u)");
                umin = ExpressionParser.Parse("0");
                umax = ExpressionParser.Parse("pi");
                vmin = ExpressionParser.Parse("0");
                vmax = ExpressionParser.Parse("2pi");
                break;
            case "Dyz":
                x = ExpressionParser.Parse("(15^(1/2)/2*sin(u)*cos(u)*1/pi^(1/2)*cos(v))^2*sin(u)*cos(v)");
                y = ExpressionParser.Parse("(15^(1/2)/2*sin(u)*cos(u)*1/pi^(1/2)*cos(v))^2*sin(u)*sin(v)");
                z = ExpressionParser.Parse("(15^(1/2)/2*sin(u)*cos(u)*1/pi^(1/2)*cos(v))^2*cos(u)");
                umin = ExpressionParser.Parse("0");
                umax = ExpressionParser.Parse("pi");
                vmin = ExpressionParser.Parse("0");
                vmax = ExpressionParser.Parse("2pi");
                break;
            case "Dxy":
                x = ExpressionParser.Parse("(15^(1/2)/4*sin(u)^2*1/pi^(1/2)*sin(2v))^2*sin(u)*cos(v)");
                y = ExpressionParser.Parse("(15^(1/2)/4*sin(u)^2*1/pi^(1/2)*sin(2v))^2*sin(u)*sin(v)");
                z = ExpressionParser.Parse("(15^(1/2)/4*sin(u)^2*1/pi^(1/2)*sin(2v))^2*cos(u)");
                umin = ExpressionParser.Parse("0");
                umax = ExpressionParser.Parse("pi");
                vmin = ExpressionParser.Parse("0");
                vmax = ExpressionParser.Parse("2pi");
                break;
            case "Dx\x00B2-y\x00B2":
                goto case "Dx2-y2";
            case "Dx2-y2":
                x = ExpressionParser.Parse("(15^(1/2)/4*sin(u)^2*1/pi^(1/2)*cos(2v))^2*sin(u)*cos(v)");
                y = ExpressionParser.Parse("(15^(1/2)/4*sin(u)^2*1/pi^(1/2)*cos(2v))^2*sin(u)*sin(v)");
                z = ExpressionParser.Parse("(15^(1/2)/4*sin(u)^2*1/pi^(1/2)*cos(2v))^2*cos(u)");
                umin = ExpressionParser.Parse("0");
                umax = ExpressionParser.Parse("pi");
                vmin = ExpressionParser.Parse("0");
                vmax = ExpressionParser.Parse("2pi");
                break;
            case "Fz\x00B3":
                goto case "Fz3";
            case "Fz3":
                x = ExpressionParser.Parse("(7^(1/2)/4*(5*((cos(u))^2)-3)*cos(u)*1/pi^(1/2))^2*sin(u)*cos(v)");
                y = ExpressionParser.Parse("(7^(1/2)/4*(5*((cos(u))^2)-3)*cos(u)*1/pi^(1/2))^2*sin(u)*sin(v)");
                z = ExpressionParser.Parse("(7^(1/2)/4*(5*((cos(u))^2)-3)*cos(u)*1/pi^(1/2))^2*cos(u)");
                umin = ExpressionParser.Parse("0");
                umax = ExpressionParser.Parse("pi");
                vmin = ExpressionParser.Parse("0");
                vmax = ExpressionParser.Parse("2pi");
                break;
            case "Fxz\x00B2":
                goto case "Fxz2";
            case "Fxz2":
                x = ExpressionParser.Parse("(42^(1/2)/8*(5*((cos(u))^2)-1)*sin(u)*sin(v)*1/pi^(1/2))^2*sin(u)*cos(v)");
                y = ExpressionParser.Parse("(42^(1/2)/8*(5*((cos(u))^2)-1)*sin(u)*sin(v)*1/pi^(1/2))^2*sin(u)*sin(v)");
                z = ExpressionParser.Parse("(42^(1/2)/8*(5*((cos(u))^2)-1)*sin(u)*sin(v)*1/pi^(1/2))^2*cos(u)");
                umin = ExpressionParser.Parse("0");
                umax = ExpressionParser.Parse("pi");
                vmin = ExpressionParser.Parse("0");
                vmax = ExpressionParser.Parse("2pi");
                break;
            case "Fyz\x00B2":
                goto case "Fyz2";
            case "Fyz2":
                x = ExpressionParser.Parse("(42^(1/2)/8*(5*((cos(u))^2)-1)*sin(u)*cos(v)*1/pi^(1/2))^2*sin(u)*cos(v)");
                y = ExpressionParser.Parse("(42^(1/2)/8*(5*((cos(u))^2)-1)*sin(u)*cos(v)*1/pi^(1/2))^2*sin(u)*sin(v)");
                z = ExpressionParser.Parse("(42^(1/2)/8*(5*((cos(u))^2)-1)*sin(u)*cos(v)*1/pi^(1/2))^2*cos(u)");
                umin = ExpressionParser.Parse("0");
                umax = ExpressionParser.Parse("pi");
                vmin = ExpressionParser.Parse("0");
                vmax = ExpressionParser.Parse("2pi");
                break;
            case "Fxyz":
                x = ExpressionParser.Parse("(105^(1/2)/4*sin(u)^2*cos(u)*cos(2v)*1/pi^(1/2))^2*sin(u)*cos(v)");
                y = ExpressionParser.Parse("(105^(1/2)/4*sin(u)^2*cos(u)*cos(2v)*1/pi^(1/2))^2*sin(u)*sin(v)");
                z = ExpressionParser.Parse("(105^(1/2)/4*sin(u)^2*cos(u)*cos(2v)*1/pi^(1/2))^2*cos(u)");
                umin = ExpressionParser.Parse("0");
                umax = ExpressionParser.Parse("pi");
                vmin = ExpressionParser.Parse("0");
                vmax = ExpressionParser.Parse("2pi");
                break;
            case "Fz\x0028x\x00B2-y\x00B2\x0029":
                goto case "Fzx2y2";
            case "Fzx2y2":
                x = ExpressionParser.Parse("(105^(1/2)/4*sin(u)^2*cos(u)*sin(2v)*1/pi^(1/2))^2*sin(u)*cos(v)");
                y = ExpressionParser.Parse("(105^(1/2)/4*sin(u)^2*cos(u)*sin(2v)*1/pi^(1/2))^2*sin(u)*sin(v)");
                z = ExpressionParser.Parse("(105^(1/2)/4*sin(u)^2*cos(u)*sin(2v)*1/pi^(1/2))^2*cos(u)");
                umin = ExpressionParser.Parse("0");
                umax = ExpressionParser.Parse("pi");
                vmin = ExpressionParser.Parse("0");
                vmax = ExpressionParser.Parse("2pi");
                break;
            case "Fx\x0028x\x00B2-3y\x00B2\x0029":
                goto case "Fxx23y2";
            case "Fxx23y2":
                x = ExpressionParser.Parse("(70^(1/2)/8*sin(u)^3*sin(3v)*1/pi^(1/2))^2*sin(u)*cos(v)");
                y = ExpressionParser.Parse("(70^(1/2)/8*sin(u)^3*sin(3v)*1/pi^(1/2))^2*sin(u)*sin(v)");
                z = ExpressionParser.Parse("(70^(1/2)/8*sin(u)^3*sin(3v)*1/pi^(1/2))^2*cos(u)");
                umin = ExpressionParser.Parse("0");
                umax = ExpressionParser.Parse("pi");
                vmin = ExpressionParser.Parse("0");
                vmax = ExpressionParser.Parse("2pi");
                break;
            case "Fy\x00283x\x00B2-y\x00B2\x0029":
                goto case "Fy3x2y2";
            case "Fy3x2y2":
                x = ExpressionParser.Parse("(70^(1/2)/8*sin(u)^3*cos(3v)*1/pi^(1/2))^2*sin(u)*cos(v)");
                y = ExpressionParser.Parse("(70^(1/2)/8*sin(u)^3*cos(3v)*1/pi^(1/2))^2*sin(u)*sin(v)");
                z = ExpressionParser.Parse("(70^(1/2)/8*sin(u)^3*cos(3v)*1/pi^(1/2))^2*cos(u)");
                umin = ExpressionParser.Parse("0");
                umax = ExpressionParser.Parse("pi");
                vmin = ExpressionParser.Parse("0");
                vmax = ExpressionParser.Parse("2pi");
                break;
        }

        expressionSet.AddExpression(ExpressionSet.ExpOptions.X, x);
        expressionSet.AddExpression(ExpressionSet.ExpOptions.Y, y);
        expressionSet.AddExpression(ExpressionSet.ExpOptions.Z, z);
        expressionSet.AddRange("t", tmin, tmax);
        expressionSet.AddRange("u", umin, umax);
        expressionSet.AddRange("v", vmin, vmax);
        expressionSet.AddRange("w", wmin, wmax);
        densManager.PresetPressed();
    }
}
