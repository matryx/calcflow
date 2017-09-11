using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class FlexRayCastHighlight : MonoBehaviour {
    FlexActionableComponent flexActionableComponent;
    int numhitters = 0;

    void Start()
    {
        flexActionableComponent = GetComponent<FlexActionableComponent>();
        RayCastReceiver receiver = transform.Find("Body").GetComponent<RayCastReceiver>();

        receiver.OnRayCastStart += OnRayCastStart;
        receiver.OnRayCastEnd += OnRayCastEnd;
    }

    void OnRayCastEnd(RayCastSender sender)
    {
        numhitters--;
    }

    void OnRayCastStart(RayCastSender sender)
    {
        print("raycast Start. NumHitters: " + numhitters);
        numhitters++;
    }

    private void Update()
    {
        if (numhitters > 0)
        {
            if (flexActionableComponent.State == 0)
            {
                flexActionableComponent.SetState(1);
            }
        }
        else
        {
            if (flexActionableComponent.State == 1)
            {
                flexActionableComponent.SetState(0);
            }
        }

    }

}
