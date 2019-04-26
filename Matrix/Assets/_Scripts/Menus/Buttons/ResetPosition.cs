using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using CalcFlowUI;

public class ResetPosition : QuickButton
{
    public Transform player;
    Transform avatar;
    private Vector3 startposition;

    private void Update()
    {
       
    }

    protected override void Start()
    {
        base.Start();
        avatar = AvatarSelector.Avatar.transform;
        startposition = avatar.position;
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        print("resetting position");

        if (SoundFXManager.instance != null)
            SoundFXManager.instance.PlayTeleportFX();

        avatar.position = new Vector3(startposition.x + avatar.position.x - player.position.x,
                                       avatar.position.y,
                                       startposition.z + avatar.position.z - player.position.z);

    }

    protected override void ButtonExitBehavior(GameObject other)
    {
    }
}
