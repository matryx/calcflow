using UnityEngine;
using System.Collections;

public class SoundFXManager : MonoBehaviour {

    public static SoundFXManager instance = null;

    AudioSource grabFX, clickFX, missFX, teleportFX, welcomeVO, vo;

    void Awake()
    {
        if(instance == null)
        {
            instance = this;
        }
        else if(instance != this)
        {
            Destroy(gameObject);
        }

        DontDestroyOnLoad(gameObject);

        grabFX = transform.Find("GrabFX").GetComponent<AudioSource>();
        clickFX = transform.Find("ClickFX").GetComponent<AudioSource>();
        missFX = transform.Find("MissFX").GetComponent<AudioSource>();
        teleportFX = transform.Find("TeleportFX").GetComponent<AudioSource>();
    }

    void Start()
    {
        GameObject go = new GameObject("Voice");
        //AudioSource vo = go.AddComponent<AudioSource>();
        vo = go.AddComponent<AudioSource>();
        vo.clip = Resources.Load("home-may") as AudioClip;
        vo.Play();
    }

    void Update()
    {
        if (!GameOptions.instance.options[2] && vo.GetComponent<AudioSource>().isPlaying)
        {
            vo.GetComponent<AudioSource>().Stop();
        }
    }

    public void PlayGrabFX()
    {
        if(GameOptions.instance.options[1])
            grabFX.Play();
    }

    public void PlayTeleportFX()
    {
        if (GameOptions.instance.options[1])
            teleportFX.Play();
    }

    public void PlayClickFX()
    {
        if (GameOptions.instance.options[1])
            clickFX.Play();
    }

    public void PlayMissFX()
    {
        if (GameOptions.instance.options[1])
            missFX.Play();
    }
}
