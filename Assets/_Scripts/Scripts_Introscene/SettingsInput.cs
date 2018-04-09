using UnityEngine;
using System.Collections;
using System.Collections.Generic;

internal class SettingsInputResponder : FlexMenu.FlexMenuResponder
{
    internal bool isReady = false;


    public SettingsInputResponder()
    {
        
    }

    public void initialize()
    {
    }

    public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
    {
        bool option;
        //SettingsInput.print("button pressed");
        switch (sender.name)
        {
            case "SoundFX":
                option = !GameOptions.instance.options[(int)GameOptions.OPTIONS.SoundFX];
                GameOptions.instance.options[(int)GameOptions.OPTIONS.SoundFX] = !option;
                break;
            case "TextDescriptions":
                option = !GameOptions.instance.options[(int)GameOptions.OPTIONS.TextDescriptions];
                GameOptions.instance.options[(int)GameOptions.OPTIONS.TextDescriptions] = !option;
                break;
            case "VoiceTutorial":
                option = !GameOptions.instance.options[(int)GameOptions.OPTIONS.VoiceOver];
                GameOptions.instance.options[(int)GameOptions.OPTIONS.VoiceOver] = !option;
                break;
            default:
                return;
        }
        if (option)
        {
            sender.SetState(1);
        }
        else
        {
            sender.SetState(0);
        }


    }
    public void Flex_ActionEnd(string name, FlexActionableComponent sender, GameObject collider)
    {
    }

}

public class SettingsInput : MonoBehaviour
{

    internal static void print(string l)
    {
        print(l);
    }

    public FlexMenu keyboard;

    SettingsInputResponder responder;

    // Use this for initialization
    void Start()
    {
        responder = new SettingsInputResponder();
        keyboard.RegisterResponder(responder);

    }

    // Update is called once per frame
    void Update()
    {

    }



}


