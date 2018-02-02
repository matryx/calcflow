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
/* Making changes to FlexMenu.cs */

using UnityEngine;
using System.Collections.Generic;
using System;
using VoxelBusters.RuntimeSerialization;

[RuntimeSerializable(typeof(MonoBehaviour), true, true)]
public class FlexMenu : MonoBehaviour
{
    private Dictionary<string, FlexPanelComponent> panels = new Dictionary<string, FlexPanelComponent>();
#if UNITY_EDITOR
    [SerializeField]
    private List<GameObject> panelList = new List<GameObject>();
#endif
    public interface FlexMenuResponder
    {
        void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider);
        void Flex_ActionEnd(string name, FlexActionableComponent sender, GameObject collider);
    }
    [SerializeField]
    private FlexMenuResponder responder;

    void Start()
    {
        ResetPanels();
        AddAllChildPanels();
    }

    public void ResetPanels()
    {
        panels.Clear();
    }

    public void AddPanel(FlexPanelComponent panel)
    {
        panels.Add(panel.gameObject.name, panel);
#if UNITY_EDITOR
        panelList.Add(panel.gameObject);
#endif
        panel.SetMenu(this);
    }

    public void AddAllChildPanels()
    {
        foreach (FlexPanelComponent cFPC in GetComponentsInChildren<FlexPanelComponent>(true))
        {
            if (!panels.ContainsKey(cFPC.name))
            {
                AddPanel(cFPC);
            }
        }
    }

    public void RegisterResponder(FlexMenuResponder responder)
    {
        //print("responder " + responder);
        this.responder = responder;
    }

    public void CloseAllPanels()
    {
        foreach (FlexPanelComponent panel in panels.Values)
        {
            panel.gameObject.SetActive(false);
        }
    }

    public void OpenPanel(string name)
    {
        panels[name].gameObject.SetActive(true);
    }

    public void ClosePanel(string name)
    {
        panels[name].gameObject.SetActive(false);
    }

    public void EndAction(string name, FlexActionableComponent sender, GameObject collider)
    {
        if (responder != null) responder.Flex_ActionEnd(name, sender, collider);
    }

    public void StartAction(string name, FlexActionableComponent sender, GameObject collider)
    {
        if (responder != null) responder.Flex_ActionStart(name, sender, collider);
    }
}