using UnityEngine;
using System.Collections;
using System;
using UnityEngine.SceneManagement;
using CalcFlowUI;


public class LoadSceneOnTouch : QuickButton
{

    public int targetSceneIndex;
    public bool reset;

    private void Update()
    {

    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
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
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
    }
}
