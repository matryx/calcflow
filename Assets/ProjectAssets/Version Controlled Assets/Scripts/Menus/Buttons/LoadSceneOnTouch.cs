using UnityEngine;
using System.Collections;
using System;
using UnityEngine.SceneManagement;
using CalcFlowUI;


public class LoadSceneOnTouch : QuickButton {

    public int targetSceneIndex;
    public bool reset;
    private bool ready;

    private float time = 0.00f;
    public float cooldown = 2.0f;

    private void Update()
    {
        if (time < cooldown)
        {
            time += Time.deltaTime;
        }
        else
        {
            ready = true;
        }

    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        if (!ready) return;

        if (reset)
        {
            if (SoundFXManager.instance != null)
                SoundFXManager.instance.PlayTeleportFX();
            UnityEngine.SceneManagement.SceneManager.LoadScene(SceneManager.GetActiveScene().name);
        }
        else
        {
            if (SoundFXManager.instance != null)
                SoundFXManager.instance.PlayTeleportFX();
            UnityEngine.SceneManagement.SceneManager.LoadScene(targetSceneIndex);
        }


        ready = false;
        time = 0.00f;
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
    }
}
