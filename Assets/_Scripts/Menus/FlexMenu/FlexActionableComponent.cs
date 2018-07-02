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
using VoxelBusters.RuntimeSerialization;

[RuntimeSerializable(typeof(MonoBehaviour), true, true)]
abstract public class FlexActionableComponent : MonoBehaviour
{

    [NonRuntimeSerializedField]
    protected Action<FlexActionableComponent, GameObject> exitCallback;
    [NonRuntimeSerializedField]
    protected Action<FlexActionableComponent, GameObject> stayCallback;
    [NonRuntimeSerializedField]
    protected Action<FlexActionableComponent, GameObject> enterCallback;

    #region Properties
    [RuntimeSerializeField]
    private int state;
    public int State
    {
        get
        {
            return state;
        }
        set
        {
            if (state == value) return;

            int _old = state;
            state = value;
            StateChanged(_old, value);
        }
    }
    
    [RuntimeSerializeField]
    private FlexPanelComponent panel;

    #endregion

    // This needs to be implemented by a subclass
    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

    //will be called on setup
    protected abstract void AssembleComponent();
    //will be called whenever state changes
    protected abstract void StateChanged(int _old, int _new);
    //will be called whenever button is removed from panel
    protected abstract void DisassembleComponent();
    // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

    public void SetupComponent(FlexPanelComponent panel,
                       Action<FlexActionableComponent, GameObject> startAction,
                       Action<FlexActionableComponent, GameObject> endAction)
    {
        this.panel = panel;
        SetEnterCallback(startAction);

        SetExitCallback(endAction);

        AssembleComponent();
    }



    public void DismantleComponent()
    {
        SetEnterCallback(null);
        SetExitCallback(null);
        DisassembleComponent();
    }

    #region Setters
    public void SetState(int _new)
    {
        State = _new;
    }

    public void SetExitCallback(System.Action<FlexActionableComponent, GameObject> exitAction)
    {
        this.exitCallback = exitAction;
    }

    public void SetStayCallback(System.Action<FlexActionableComponent, GameObject> OnCollisionEnter)
    {
        this.stayCallback = OnCollisionEnter;
    }

    public void SetEnterCallback(System.Action<FlexActionableComponent, GameObject> enterAction)
    {
        this.enterCallback = enterAction;
    }
    #endregion

}
