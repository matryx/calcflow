/*************************************************************************
 * 
 * NANOVR CONFIDENTIAL
 * __________________
 * 
 *  [2015] - [2016] NANOVR Incorporated 
 *  All Rights Reserved.
 * 
 * NOTICE:  All information contained herein is, and remains
 * the property of NANOVR Incorporated and its suppliers,
 * if any.  The intellectual and technical concepts contained
 * herein are proprietary to NANOVR Incorporated
 * and its suppliers and may be covered by U.S. and Foreign Patents,
 * patents in process, and are protected by trade secret or copyright law.
 * Dissemination of this information or reproduction of this material
 * is strictly forbidden unless prior written permission is obtained
 * from NANOVR Incorporated.
 */
using UnityEngine;
using System.Collections;
using System;

public class FlexButtonComponent : FlexActionableComponent
{
    public Color passiveColor;
    public Color hoveringColor;
    public Color selectedColor;
    public Color disabledColor;

    private void Awake()
    {
        State = -1;
        SetState(0);
    }

    protected override void DisassembleComponent()
    {
        VirtualButton virtualButton = GetComponent<VirtualButton>();
        if (virtualButton != null)
        {
            virtualButton.OnButtonEnter -= startAction;
            virtualButton.OnButtonExit -= endAction;
        }

        TouchRayButton rayTouchButton = GetComponent<TouchRayButton>();
        if (rayTouchButton != null)
        {
            rayTouchButton.OnButtonEnter -= startAction;
            rayTouchButton.OnButtonExit -= endAction;
            return;
        }


        RayCastButton rcButton = GetComponent<RayCastButton>();
        if (rcButton != null)
        {
            rcButton.OnButtonEnter -= startAction;
            rcButton.OnButtonExit -= endAction;
        }

        TouchButton touchButton = GetComponent<TouchButton>();
        if (touchButton != null)
        {
            touchButton.OnButtonEnter -= startAction;
            touchButton.OnButtonExit -= endAction;
        }
    }

    protected override void AssembleComponent()
    {
        VirtualButton virtualButton = GetComponent<VirtualButton>();
        if (virtualButton != null)
        {
            virtualButton.OnButtonEnter += startAction;
            virtualButton.OnButtonExit += endAction;
        }

        TouchRayButton rayTouchButton = GetComponent<TouchRayButton>();
        if (rayTouchButton != null)
        {
            rayTouchButton.OnButtonEnter += startAction;
            rayTouchButton.OnButtonExit += endAction;
            return;
        }


        RayCastButton rcButton = GetComponent<RayCastButton>();
        if (rcButton != null)
        {
            rcButton.OnButtonEnter += startAction;
            rcButton.OnButtonExit += endAction;
        }

        TouchButton touchButton = GetComponent<TouchButton>();
        if (touchButton != null)
        {
            touchButton.OnButtonEnter += startAction;
            touchButton.OnButtonExit += endAction;
        }

        if (State != 2)
            State = 0;
    }

    protected override void StateChanged(int _old, int _new)
    {
        if (_new == -1)
        {
            transform.Find("Body").GetComponent<Renderer>().material.color = disabledColor;
        } else if (_new == 1)
        {
            transform.Find("Body").GetComponent<Renderer>().material.color = hoveringColor;
        }
        else if (_new == 2)
        {
            transform.Find("Body").GetComponent<Renderer>().material.color = selectedColor;
        }
        else
        {
            transform.Find("Body").GetComponent<Renderer>().material.color = passiveColor;
        }
    }

    void startAction(GameObject gameobject)
    {
        if (State >= 0)
        {
            enterCallback(this, gameObject);
        }
    }

    void endAction(GameObject gameobject)
    {
        if (State >= 0)
        {
            exitCallback(this, gameObject);
        }
    }
}

