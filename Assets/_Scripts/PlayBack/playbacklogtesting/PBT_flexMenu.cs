using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using VoxelBusters.RuntimeSerialization;

[RuntimeSerializable(typeof(MonoBehaviour), true,true)]
public class PBT_flexMenu : MonoBehaviour
{
    private FlexMenu keyboard;

    KeyboardInputResponder responder;

    internal class KeyboardInputResponder : FlexMenu.FlexMenuResponder
    {
        PBT_flexMenu tester;
        internal KeyboardInputResponder(PBT_flexMenu tester)
        {
            this.tester = tester;
        }

        public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
        {
            tester.HandleInput(sender.name);
        }

        public void Flex_ActionEnd(string name, FlexActionableComponent sender, GameObject collider) { }

    }
    // Use this for initialization
    void Awake()
    {
        keyboard = GetComponent<FlexMenu>();
        responder = new KeyboardInputResponder(this);
        keyboard.RegisterResponder(responder);
    }

    void HandleInput(string name)
    {
		print ("FlexMenu hit by " + name);
    }

    // Update is called once per frame
    void Update()
    {

    }
}
