using UnityEngine;
using System.Collections;

public class UtilityOptions : MonoBehaviour
{
    Transform vo;
    AudioSource audioSource;
    bool alreadyPlayed = false;

    void Start()
    {
        //NOTE: GameOptions is buggy

        #region old code with GameOptions
        //if (transform.Find("Description"))
        //{
        //    if (GameOptions.instance.options[0])
        //    {
        //        transform.Find("Description").gameObject.SetActive(true);
        //    }
        //    else
        //    {
        //        transform.Find("Description").gameObject.SetActive(false);
        //    }
        //}

        //if (vo = transform.Find("Voiceover"))
        //{
        //    if (GameOptions.instance.options[2])
        //    {
        //        audioSource = vo.GetComponent<AudioSource>();
        //        //transform.FindChild("Voiceover").GetComponent<AudioSource>().Play();
        //        audioSource.Play();
        //        print("audiosource initialized");
        //    }
        //}
        #endregion

        audioSource = transform.Find("Voiceover").GetComponent<AudioSource>();
    }

    void Update()
    {
        
        if (!SettingsVariables.narration && audioSource && audioSource.isPlaying)
        {
            audioSource.Stop();
            alreadyPlayed = false;
        }
        else if (SettingsVariables.narration && audioSource && 
                 !audioSource.isPlaying && !alreadyPlayed)
        {
            audioSource.volume = SettingsVariables.volume;
            audioSource.Play();
            alreadyPlayed = true;
        }
        
    }
}
