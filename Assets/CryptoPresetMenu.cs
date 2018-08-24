using System.Collections;
using System.Collections.Generic;
using System;
using System.Text;
using UnityEngine;
using Nanome.Core;
using UnityEngine.Networking;
using TMPro;

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
    LineChart lineChart;
    CandleChart candleChart;

    ScatterChart scatterChart;

    TimeSelect timeSelect;

    KeyboardMenu board;

    GameObject graph;
    public GameObject Keyboard;

    double first, second;
    string baseURL = "https://graphs2.coinmarketcap.com/currencies/";
    string currCrypto = "bitcoin", currTime = "All", toSearch;
    StringBuilder builder = new StringBuilder();
    StringBuilder customInput = new StringBuilder();

    public GameObject inputButton;
    private TextMeshPro textMesh;
    public Transform view;

    public Transform cryptoName;


    private Dictionary<string, bool> presets = new Dictionary<string, bool>();
    Scroll scroll;
    JoyStickAggregator joyStickAggregator;

    void Start()
    {
        lineChart = LineChart.GetInstance();
        candleChart = CandleChart.GetInstance();
        scatterChart = ScatterChart.GetInstance();
        timeSelect = TimeSelect.GetInstance();

        board = KeyboardMenu.GetInstance();
        scroll = GetComponentInChildren<Scroll>(true);
        joyStickAggregator = scroll.GetComponent<JoyStickAggregator>();
        //HandleInput(defaultFunction);
        KeyboardInputResponder responder = new KeyboardInputResponder(this);
        menu.RegisterResponder(responder);
        initializePresetButtons();
        getTimeStamps(currTime);
        //HandleInput(defaultFunction);
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

    public void sendSignal(string source)
    {
        HandleInput(source);
    }
    protected void HandleInput(string source)
    {
        textMesh = inputButton.GetComponent<TextMeshPro>();
        switch (source)
        {
            default:
                if (source.Equals("Enter"))
                {
                    toSearch = customInput.ToString();
                    textMesh.text = "Custom Input " + "(" + toSearch + ")";
                    webCall();
                    customInput = new StringBuilder();
                }
                else if (source.Equals("Del"))
                {
                    customInput.Remove(customInput.Length - 1, 1);
                }
                else if (source.Equals("Exit"))
                {
                    Keyboard.SetActive(false);
                }
                else
                {
                    customInput.Append(source);
                }
                break;
            //R1 -> R1
            case "Toggle":
                lineChart.gameObject.SetActive(!lineChart.gameObject.activeInHierarchy);
                candleChart.gameObject.SetActive(!candleChart.gameObject.activeInHierarchy);
                break;
            case "MTX":
                currCrypto = "matryx";
                newGraph();
                textMesh.text = "Custom Input";
                break;
            case "BTC":
                currCrypto = "bitcoin";
                newGraph();
                textMesh.text = "Custom Input";
                break;
            case "ETH":
                currCrypto = "ethereum";
                newGraph();
                textMesh.text = "Custom Input";
                break;
            case "Custom":
                // TODO: make keyboard for custom input.
                customInput = new StringBuilder();
                setKeyboardPos();
                Keyboard.SetActive(true);
                board = KeyboardMenu.GetInstance();
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
    void setKeyboardPos()
    {
        Transform Keyboardt = Keyboard.transform;
        Keyboardt.SetParent(view);
        Keyboardt.localPosition = new Vector3(0, 0, 0.7f);
        Keyboardt.localEulerAngles = new Vector3(0, 0, 0);
        Keyboardt.SetParent(null);
        Keyboardt.localEulerAngles = new Vector3(Keyboardt.localEulerAngles.x,
        Keyboardt.localEulerAngles.y, Keyboardt.localEulerAngles.z);
        //Keyboardt.localScale = Vector3.one;
    }
    void newGraph()
    {
        //        lineChart.kill();
        //        lineChart.SetURL(baseURL + currCrypto + "/" + first + "/" + second + "/");
        //        lineChart.updateGraph();

        TextMesh text = cryptoName.GetComponent<TextMesh>();
        text.text = currCrypto;

        scatterChart.createLabels = true;
        scatterChart.kill();
        scatterChart.SetURL(baseURL + currCrypto + "/" + first + "/" + second + "/");
        scatterChart.updateGraph();

        timeSelect.setCoin(currCrypto);
        timeSelect.updateTimes();
    }

    void getTimeStamps(string source)
    {
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
                first = second - getMillis(24 * 7);
                break;
            case "1m":
                currTime = source;
                first = second - getMillis(24 * 30);
                break;
            case "3m":
                currTime = source;
                first = second - getMillis(24 * 30 * 3);
                break;
            case "1yr":
                currTime = source;
                first = second - getMillis(24 * 7 * 52);
                break;
            case "All":
                currTime = source;
                first = 0;
                break;
        }

    }

    double getCurrTime()
    {
        DateTime epochstart = new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc);
        double currTime = (double)(DateTime.UtcNow - epochstart).TotalMilliseconds;
        return currTime;
    }
    double getMillis(double numHours)
    {
        return numHours * 3600 * 1000;
    }

    void webCall()
    {
        builder = new StringBuilder();
        Async obj = Async.runInCoroutine(GetJSON);
        obj.onEvent("Finished", parseJSON);
    }

    IEnumerator GetJSON(Async routine)
    {
        using (WWW www = new WWW("https://api.coinmarketcap.com/v2/listings/"))
        {
            yield return www;
            yield return www.text;
            builder.Append(www.text);
            routine.pushEvent("Finished", builder);
        }
    }

    void parseJSON(object tmp)
    {
        string data = ((StringBuilder)tmp).ToString();
        findName(data, toSearch);
    }

    void findName(string text, string search)
    {
        text = text.ToLower();
        search = search.ToLower();
        search = "\"" + search + "\"";
        int start = text.IndexOf(search) + 1;
        if (start == 0)
        {
            Keyboard.SetActive(true);
            board.notFoundError();
        }
        else
        {
            //Keyboard.SetActive(false);
            text = text.Substring(start, text.Length - 1 - start);
            int name = text.IndexOf("website_slug") + 16;
            int nameEnd = text.IndexOf("}");
            currCrypto = text.Substring(name, nameEnd - name - 10);
            newGraph();
        }
    }
}
