using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using CalcFlowUI;

public class ResetPosition : QuickButton
{
    public Transform player;
    public Transform avatar;
    private bool ready;
    private float time = 0.00f;
    public float cooldown = 2.0f;
    private Vector3 startposition;

    private void Update()
    {
        if (time < cooldown)
        {
            time += Time.deltaTime;
        } else
        {
            ready = true;
        }
       
    }

    protected override void Start()
    {
        base.Start();
        startposition = avatar.position;
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        print("resetting position");
        if (!ready) return;
        print(" 1");

        if (SoundFXManager.instance != null)
            SoundFXManager.instance.PlayTeleportFX();

        avatar.position = new Vector3(startposition.x + avatar.position.x - player.position.x,
                                       avatar.position.y,
                                       startposition.z + avatar.position.z - player.position.z);

        ready = false;
        time = 0.00f;
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
    }
}
