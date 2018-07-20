using System.Collections;
using System.Collections.Generic;
using System;
using System.Text;
using UnityEngine;
using Nanome.Core;
using UnityEngine.Networking;

public class KeyboardMenu : MonoBehaviour
{

    internal class KeyboardInputResponder : FlexMenu.FlexMenuResponder
    {
        KeyboardMenu presetMenu;
        internal KeyboardInputResponder(KeyboardMenu calcInput)
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
    public string defaultFunction = "MTX";
    CalcManager calcManager;
    LineChart chart;

    GameObject graph;

    double first, second;
    string baseURL = "https://graphs2.coinmarketcap.com/currencies/";
    string currCrypto, currTime = "1yr", toSearch;
    StringBuilder builder = new StringBuilder();

    

    private Dictionary<string, bool> presets = new Dictionary<string, bool>();
    Scroll scroll;
    JoyStickAggregator joyStickAggregator;

    void Start()
    {
        Debug.Log("Initialize");
        chart = LineChart.GetInstance();
 //       scroll = GetComponentInChildren<Scroll>(true);
//        joyStickAggregator = scroll.GetComponent<JoyStickAggregator>();
        //HandleInput(defaultFunction);
        KeyboardInputResponder responder = new KeyboardInputResponder(this);
        menu.RegisterResponder(responder);
        initializePresetButtons();
    }

    private void initializePresetButtons()
    {
        #region add to presets
        //R1 -> R1
        //presets.Add("MTX", true);

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
        switch (source)
        {
            default:
                //Debug.Log("unknown preset pressed");
                Debug.Log("Button Pressed: " + source);
                break;
            //R1 -> R1
            case "MTX":
                currCrypto = "matryx";
                break;
            case "BTC":
                currCrypto = "bitcoin";
                break;

        }
    }



}
