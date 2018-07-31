using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class RecorderMenu : MonoBehaviour
{

    internal class KeyboardInputResponder : FlexMenu.FlexMenuResponder
    {
        RecorderMenu menu;
        internal KeyboardInputResponder(RecorderMenu menu)
        {
            this.menu = menu;
        }
        public KeyboardInputResponder() { }

        public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
        {
            menu.HandleInput(sender.name);
        }

        public void Flex_ActionEnd(string name, FlexActionableComponent sender, GameObject collider) { }

    }

    public void Initialize()
    {
        KeyboardInputResponder responder = new KeyboardInputResponder(this);
        GetComponent<FlexMenu>().RegisterResponder(responder);
    }

    protected void HandleInput(string source)
    {
        switch (source)
        {

            case "Start":
                Recorder.StartRecording();
                break;
            case "Pause":
                Recorder.PauseRecording();
                break;
            case "Resume":
                Recorder.ResumeRecording();
                break;
            case "Stop":
                Recorder.EndRecording();
                break;
            default:
                print("unknown input: " + source);
                break;
        }
    }
}
