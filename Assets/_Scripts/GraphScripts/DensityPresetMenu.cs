using System.Collections;
using System.Collections.Generic;
using UnityEngine;

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
    public string defaultFunction = "3Pz";
    DensityInputManager densManager;

    [SerializeField]
    private bool s1, s2, p2, s3, p3, d3z2, d3xy, s4, p4, d4z2, d4xy, f4z3, f4xz2, f4xyz;

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
        presets.Add("3S", s3);
        presets.Add("3Pz", p3);
        presets.Add("3Dz\x00B2", d3z2);
        presets.Add("3Dxy", d3xy);
        presets.Add("4S", s4);
        presets.Add("4Pz", p4);
        presets.Add("4Dz\x00B2", d4z2);
        presets.Add("4Dxy", d4xy);
        presets.Add("4Fz\x00B3", f4z3);
        presets.Add("4Fxz\x00B2", f4xz2);
        presets.Add("4Fxyz", f4xyz);
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

                if (pair.Key == defaultFunction)
                {
                    presetButton.GetComponent<FlexActionableComponent>().SetState(2);
                }

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
            case "3S":
                x = ExpressionParser.Parse("(0.004021*(27-18*(x^2+y^2+z^2)^(1/2)+2*(x^2+y^2+z^2))*e^(-1*((x^2+y^2+z^2)^(1/2))/3))^2");
                y = ExpressionParser.Parse("0");
                z = ExpressionParser.Parse("0");
                densManager.setRange(20);
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
            case "4S":
                x = ExpressionParser.Parse("(0.006965*(1-0.75*(x^2+y^2+z^2)^(1/2)+0.125*(x^2+y^2+z^2)-0.005208*(x^2+y^2+z^2)^(3/2))*e^(-1*((x^2+y^2+z^2)^(1/2))/4))^2");
                y = ExpressionParser.Parse("0");
                z = ExpressionParser.Parse("0");
                densManager.setRange(30);
                break;
            case "4Pz":
                x = ExpressionParser.Parse("(0.039424*(1-0.25*(x^2+y^2+z^2)^(1/2)+0.0125*(x^2+y^2+z^2))*z*e^(-1*((x^2+y^2+z^2)^(1/2))/4))^2");
                y = ExpressionParser.Parse("0");
                z = ExpressionParser.Parse("0");
                densManager.setRange(34);
                break;
            case "4Dz\x00B2":
                goto case "4Dz2";
            case "4Dz2":
                x = ExpressionParser.Parse("(0.002204*(3*z^2-0.25*z^2*(x^2+y^2+z^2)^(1/2)-(x^2+y^2+z^2)+0.083333*(x^2+y^2+z^2)^(3/2))*e^(-1*((x^2+y^2+z^2)^(1/2))/4))^2");
                y = ExpressionParser.Parse("0");
                z = ExpressionParser.Parse("0");
                densManager.setRange(40);
                break;
            case "4Dxy":
                x = ExpressionParser.Parse("(0.003817*x*y*(1-0.083333*(x^2+y^2+z^2)^(1/2))*e^(-1*((x^2+y^2+z^2)^(1/2))/4))^2");
                y = ExpressionParser.Parse("0");
                z = ExpressionParser.Parse("0");
                densManager.setRange(40);
                break;
            case "4Fz\x00B3":
                goto case "4Fz3";
            case "4Fz3":
                x = ExpressionParser.Parse("(0.000082*z*(5*z^2-3*(x^2+y^2+z^2))*e^(-1*((x^2+y^2+z^2)^(1/2))/4))^2");
                y = ExpressionParser.Parse("0");
                z = ExpressionParser.Parse("0");
                densManager.setRange(40);
                break;
            case "4Fxz\x00B2":
                goto case "4Fxz2";
            case "4Fxz2":
                x = ExpressionParser.Parse("(0.000101*x*(5*z^2-(x^2+y^2+z^2))*e^(-1*((x^2+y^2+z^2)^(1/2))/4))^2");
                y = ExpressionParser.Parse("0");
                z = ExpressionParser.Parse("0");
                densManager.setRange(40);
                break;
            case "4Fxyz":
                x = ExpressionParser.Parse("(0.000636*x*y*z*e^(-1*((x^2+y^2+z^2)^(1/2))/4))^2");
                y = ExpressionParser.Parse("0");
                z = ExpressionParser.Parse("0");
                densManager.setRange(40);
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
