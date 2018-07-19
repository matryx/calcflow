using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class CryptoPresetMenu : MonoBehaviour
{

    internal class KeyboardInputResponder : FlexMenu.FlexMenuResponder
    {
        CryptoPresetMenu presetMenu;
        internal KeyboardInputResponder(CryptoPresetMenu calcInput)
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
    public string defaultFunction = "Matryx";
    CalcManager calcManager;
    LineChart chart;

    private Dictionary<string, bool> presets = new Dictionary<string, bool>();
    Scroll scroll;
    JoyStickAggregator joyStickAggregator;

    public void Initialize(CalcManager cm)
    {
        chart = LineChart.GetInstance();
        scroll = GetComponentInChildren<Scroll>(true);
        joyStickAggregator = scroll.GetComponent<JoyStickAggregator>();
        calcManager = cm;
        HandleInput(defaultFunction);
        KeyboardInputResponder responder = new KeyboardInputResponder(this);
        menu.RegisterResponder(responder);
    }


    protected void HandleInput(string source)
    {

        switch (source)
        {
            default:
                print("unknown preset pressed");
                break;
            //R1 -> R1
            case "Matryx":
                chart.SetURL("https://graphs2.coinmarketcap.com/currencies/bitcoin/1367174841000/1531512240000/");
                break;
            case "Circle":
                break;
        }

        calcManager.PresetPressed();
    }
}
