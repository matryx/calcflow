using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class LoadSceneOnClick : MonoBehaviour
{

    public int targetSceneIndex;

    public void OnMouseClick()
    {
        if (SoundFXManager.instance != null)
            SoundFXManager.instance.PlayTeleportFX();
        UnityEngine.SceneManagement.SceneManager.LoadScene(targetSceneIndex);
    }

}
