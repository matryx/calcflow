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


using System;
using UnityEngine;

public class FlexPanelFreeformComponent : FlexPanelComponent
{
    protected override void OnActionEnd(FlexActionableComponent sender, GameObject collider)
    {
    }

    protected override void OnActionStart(FlexActionableComponent sender, GameObject collider)
    {
        ClearSelection();
        if(sender.State != -1) sender.SetState(1);
    }

    // Use this for initialization
    void Start()
    {
        Actions.Clear();
        AddAllChildActions();
    }
}