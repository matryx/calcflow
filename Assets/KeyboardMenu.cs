using System.Collections;
using System.Collections.Generic;
using System;
using System.Text;
using UnityEngine;
using Nanome.Core;
using UnityEngine.Networking;
using TMPro;

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

    private static KeyboardMenu _instance;
	void Awake () {
		_instance = this;
	}

    public static KeyboardMenu GetInstance(){
	    return _instance;
	}
    public FlexMenu menu;

    public CryptoPresetMenu cryptoMenu;
    LineChart chart;
    TextMeshPro textMesh;
    public GameObject outputObject;
    StringBuilder output = new StringBuilder();


    

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
        textMesh = outputObject.GetComponent<TextMeshPro>();
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
                if(textMesh.text.Equals("Currency not found")){
                    textMesh.text = "";
                }
                cryptoMenu.sendSignal(source);
                if(source.Equals("Del")){
                    output.Remove(output.Length-1,1);
                    textMesh.text = output.ToString();
                }else if (source.Equals("Enter")){
                    output = new StringBuilder();
                    textMesh.text = output.ToString();
                }else if (source.Equals("Exit")){
                    //gameObject.SetActive(false);
                }else{
                    output.Append(source);
                    textMesh.text = output.ToString();
                }

                break;
        }
    }

    public void notFoundError(){
        textMesh = outputObject.GetComponent<TextMeshPro>();
        textMesh.text = "Currency not found";
        Debug.Log("Here!");
    }



}
