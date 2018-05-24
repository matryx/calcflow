using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using VoxelBusters.RuntimeSerialization;

[RuntimeSerializable(typeof(MonoBehaviour), true, true)]
public class VolumeControl : MonoBehaviour
{
    Vector3 lastPos;
    TextMesh volumeValue;
    private ConstraintGrabbable ball;

    AudioSource[] soundFiles;

    void Start()
    {
        if (!volumeValue)
        {
            volumeValue = transform.Find("VolumeValue").GetComponent<TextMesh>();
            if (!volumeValue)
            {
                Debug.Log("no volumeball found");
            }
        }
        soundFiles = GameObject.FindObjectsOfType(typeof(AudioSource)) as AudioSource[];

        foreach (AudioSource aud in soundFiles)
        {
            aud.volume = SettingsVariables.volume;
        }

        volumeValue.text = (SettingsVariables.volume * 100).ToString();
        ball.lastLocalPos = SettingsVariables.volumeBallLocalPos;
    }

    private void adjustVolume()
    {
        float roundedX = (float)System.Math.Round(ball.lastLocalPos.x, 2);
        int newVolume = (int)(roundedX * 100) % 2 == 0 ?
                        (int)(roundedX / 0.02f) : (int)Mathf.Round(roundedX / 0.02f);

        volumeValue.text = newVolume.ToString();
        SettingsVariables.volume = ((float)newVolume) / 100f;

        foreach (AudioSource aud in soundFiles)
        {
            aud.volume = SettingsVariables.volume;
        }
    }

    void Update()
    {
        if (lastPos != ball.lastLocalPos)
        {
            adjustVolume();
            SettingsVariables.volumeBallLocalPos = ball.lastLocalPos;
        }

        lastPos = ball.lastLocalPos;
    }

    private void OnEnable()
    {
        if (ball)
        {
            ball.gameObject.GetComponent<Collider>().enabled = true;
            ball.gameObject.GetComponent<MeshRenderer>().enabled = true;
        }
    }

    private void OnDisable()
    {
        if (ball)
        {
            ball.gameObject.GetComponent<Collider>().enabled = false;
            ball.gameObject.GetComponent<MeshRenderer>().enabled = false;
        }
    }
}
