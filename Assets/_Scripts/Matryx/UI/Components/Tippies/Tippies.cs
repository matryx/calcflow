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
    
    public static Tippy SpawnTippy(string text, float textSize, TMPro.TextAlignmentOptions textAlignment, Vector2 size, float lifetime, Transform location, Vector3 offset, float fadeInDuration, float fadeOutDuration, Tippy.MovementMode mode)
    {
        GameObject tippyObject = Instantiate(Resources.Load("Prefabs/Tippy", typeof(GameObject))) as GameObject;
        Tippy tippy = tippyObject.GetComponent<Tippy>();
        tippy.dimensions = new Vector3(size.x, size.y, 1f);
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
}
