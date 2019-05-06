using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Tippies : MonoBehaviour {
    public static Tippies Instance { get; private set; }
    static Dictionary<string, Tippy> tippies;

    public void Start()
    {
        if (Instance == null)
        {
            Instance = this;
            tippies = new Dictionary<string, Tippy>();
        }
    }
    
    public static Tippy SpawnTippy(string text, float textSize, TMPro.TextAlignmentOptions textAlignment, Vector3 size, float lifetime, Transform location, Vector3 offset, float fadeInDuration, float fadeOutDuration, Tippy.MovementMode mode, bool exclusive = false)
    {
        if(exclusive && tippies.ContainsKey(text))
        {
            tippies[text].lifetime = lifetime;
            return tippies[text];
        }

        GameObject tippyObject = Instantiate(Resources.Load("Prefabs/Tippy", typeof(GameObject))) as GameObject;
        Tippy tippy = tippyObject.GetComponent<Tippy>();
        tippy.dimensions = size;
        tippy.lifetime = lifetime;
        tippy.location = location;
        tippy.offset = offset;
        tippy.fadeInDuration = fadeInDuration;
        tippy.fadeOutDuration = fadeOutDuration;
        tippy.mode = mode;
        tippy.Start();
        tippy.setText(text, textSize, textAlignment);
        tippy.transform.SetParent(Instance.transform);

        if (!tippies.ContainsKey(text))
        {
            tippies.Add(text, tippy);
        }

        return tippy;
    }

    public static Tippy HeadsetModal(string text)
    {
        return SpawnTippy(text, 4f, TMPro.TextAlignmentOptions.Center, new Vector2(6, 1f), 3f, AvatarSelector.centerEye, new Vector3(0f, 0f, 0.4f), 0.5f, 0.5f, Tippy.MovementMode.Soft, true);
    }

    public static void DestroyTippy(string text)
    {
        if(tippies.ContainsKey(text))
        {
            Tippy toRemove = tippies[text];
            tippies.Remove(text);
            if (toRemove != null)
            {
                Destroy(toRemove.gameObject);
            }
        }
    }

    public static void FadeDestroyTippy(string text)
    {
        if(tippies.ContainsKey(text))
        {
            if (tippies[text] != null)
            {
                Tippy tippy = tippies[text];
                tippies[text].fadeEarly(0.2f, (obj) =>
                {
                    Destroy(tippy.gameObject);
                });
            }

            tippies.Remove(text);
        }
    }
}
