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
using System.Collections.Generic;

abstract public class FlexPanelComponent : MonoBehaviour
{
    protected FlexMenu menu;
    private List<FlexActionableComponent> actions = new List<FlexActionableComponent>();

    public List<FlexActionableComponent> Actions
    {
        get
        {
            return actions;
        }
    }

    public void SetMenu(FlexMenu menu)
    {
        this.menu = menu;
    }

    public FlexMenu GetMenu()
    {
        return menu;
    }

    public void AddAllChildActions()
    {
        foreach (FlexActionableComponent action in GetComponentsInChildren<FlexActionableComponent>(true))
        {
            action.SetupComponent(this, StartAction, EndAction);
            actions.Add(action);
        }
        //for (int i = 0; i < transform.childCount; i++)
        //{
        //    FlexActionableComponent action = transform.GetChild(i).GetComponent<FlexActionableComponent>();
        //    if (action != null)
        //    {
        //        action.SetupComponent(this, StartAction, EndAction);
        //        actions.Add(action);
        //    }
        //}
    }

    public void RemoveAllActions()
    {
        foreach (FlexActionableComponent action in actions)
        {
            action.DismantleComponent();
        }

        actions.Clear();
    }

    public virtual void AddAction(FlexActionableComponent action)
    {
        if (action != null)
        {
            action.SetupComponent(this, StartAction, EndAction);
            actions.Add(action);
        }
    }

    public void RemoveAction(FlexActionableComponent action)
    {
        if (action != null)
        {
            action.DismantleComponent();
            actions.Remove(action);
        }
    }

    public void ClearSelection()
    {
        foreach (FlexActionableComponent action in Actions)
        {
            if (action.State == 2)
                action.SetState(0);
        }
    }

    protected virtual void StartAction(FlexActionableComponent sender, GameObject collider)
    {
        if (menu != null)
        {
            menu.StartAction(name, sender, collider);
            OnActionStart(sender, collider);
        }
    }

    protected abstract void OnActionStart(FlexActionableComponent sender, GameObject collider);
    protected abstract void OnActionEnd(FlexActionableComponent sender, GameObject collider);

    protected virtual void EndAction(FlexActionableComponent sender, GameObject collider)
    {
        if (menu != null)
        {
            menu.EndAction(name, sender, collider);
            OnActionEnd(sender, collider);
        }
    }

}
