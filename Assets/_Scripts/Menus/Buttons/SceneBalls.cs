using UnityEngine;
using System.Collections;
using System;
using UnityEngine.SceneManagement;
using CalcFlowUI;

public class SceneBalls : QuickButton
{

    public int targetSceneIndex;

    public void changeScene()
    {

        if (SoundFXManager.instance != null)
            SoundFXManager.instance.PlayTeleportFX();
        UnityEngine.SceneManagement.SceneManager.LoadScene(targetSceneIndex);
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {

        changeScene();
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
    }
}
