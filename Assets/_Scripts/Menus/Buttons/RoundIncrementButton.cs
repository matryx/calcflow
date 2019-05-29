using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class RoundIncrementButton : QuickButton
{

    [SerializeField]
    bool increment = true;

    TournamentMenu tournamentMenu;

    protected override void Start()
    {
        base.Start();
        tournamentMenu = transform.parent.parent.GetComponent<TournamentMenu>();
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        if(increment)
        {
            
        }
        else
        {
            
        }
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
    }
}
