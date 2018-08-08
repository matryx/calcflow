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

    void Awake()
    {
        KeyboardInputResponder responder = new KeyboardInputResponder(this);
        GetComponent<FlexMenu>().RegisterResponder(responder);
    }

    protected void HandleInput(string source)
    {
        switch (source)
        {

            case "Start":
                RecordAndReplayManager._instance.EditorRecord = true;
                //Recorder.StartRecording();
                break;
            case "Pause":
                RecordAndReplayManager._instance.EditorPause = true;
                //Recorder.PauseRecording();
                break;
            case "Resume":
                RecordAndReplayManager._instance.EditorPause = false;
                //Recorder.ResumeRecording();
                break;
            case "Stop":
                RecordAndReplayManager._instance.EditorRecord = false;
                //Recorder.EndRecording();
                break;
            default:
                print("unknown input: " + source);
                break;
        }
    }
}
