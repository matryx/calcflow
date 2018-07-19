using System.Collections;
using System.Collections.Generic;
using System;
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
    public string defaultFunction = "MTX";
    CalcManager calcManager;
    LineChart chart;

    GameObject graph;

    double first, second;
    string baseURL = "https://graphs2.coinmarketcap.com/currencies/";
    string currCrypto, currTime = "1yr";

    private Dictionary<string, bool> presets = new Dictionary<string, bool>();
    Scroll scroll;
    JoyStickAggregator joyStickAggregator;

    void Start()
    {
        Debug.Log("Initialize");
        chart = LineChart.GetInstance();
        scroll = GetComponentInChildren<Scroll>(true);
        joyStickAggregator = scroll.GetComponent<JoyStickAggregator>();
        //HandleInput(defaultFunction);
        KeyboardInputResponder responder = new KeyboardInputResponder(this);
        menu.RegisterResponder(responder);
        initializePresetButtons();
        getTimeStamps(currTime);
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
                Debug.Log("unknown preset pressed");
                break;
            //R1 -> R1
            case "MTX":
                currCrypto = "matryx";
                newGraph();
                break;
            case "BTC":
                currCrypto = "bitcoin";
                newGraph();
                break;
            case "ETH":
                currCrypto = "ethereum";
                newGraph();
                break;
            case "1d":
                getTimeStamps(source);
                newGraph();
                break;
            case "7d":
                 getTimeStamps(source);
                 newGraph();
                break;
            case "1m":
                 getTimeStamps(source);
                 newGraph();
                break;
            case "3m":
                 getTimeStamps(source);
                 newGraph();
                break;
            case "1yr":
                getTimeStamps(source);
                newGraph();
                break;
            case "All":
                getTimeStamps(source);
                newGraph();
                break;
        }
    }

    void newGraph(){
            chart.kill();
            chart.SetURL(baseURL + currCrypto + "/" + first + "/" + second + "/");
            Debug.Log("URL: " + baseURL + currCrypto + "/" + first + "/" + second + "/");
            chart.updateGraph();
    }

    void getTimeStamps(string source){
        second = Math.Round(getCurrTime());
        switch (source)
        {
            default:
                Debug.Log("unknown preset pressed");
                break;
            //R1 -> R1
            case "1d":
                currTime = source;
                first = second - getMillis(24);
                break;
            case "7d":
                currTime = source;
                 first = second - getMillis(24*7);
                break;
            case "1m":
                currTime = source;
                 first = second - getMillis(24*30);
                break;
            case "3m":
                currTime = source;
                first = second - getMillis(24*30*3);
                break;
            case "1yr":
                currTime = source;
                first = second - getMillis(24*7*52);
                break;
            case "All":
                currTime = source;
                first = 0;
                break;
        }

        Debug.Log("FIRST: " + first + ", " + "SECOND: " + second);
    }

    double getCurrTime(){
        DateTime epochstart = new DateTime(1970,1,1,0,0,0, DateTimeKind.Utc);
        double currTime = (double)(DateTime.UtcNow - epochstart).TotalMilliseconds;
        return currTime;
    }

    double getMillis(double numHours){
        return numHours * 3600 * 1000;
    }
}
