using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ReplayerMenu : MonoBehaviour
{

    internal class KeyboardInputResponder : FlexMenu.FlexMenuResponder
    {
        ReplayerMenu menu;
        internal KeyboardInputResponder(ReplayerMenu menu)
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
                //Replayer.StartReplaying();
                RecordAndReplayManager._instance.EditorReplay = true;
                break;
            case "Pause":
                //Replayer.PauseReplaying();
                //RecordAndReplayManager._instance.EditorPause = true;
                break;
            case "Resume":
                //RecordAndReplayManager._instance.EditorPause = false;
                //Replayer.ResumeReplaying();
                break;
            case "Stop":
                RecordAndReplayManager._instance.EditorReplay = false;
                //Replayer.StopReplaying();
                break;
            default:
                print("unknown input: " + source);
                break;
        }
    }
}
