using Nanome.Core;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Tippy : MonoBehaviour {

    public float lifetime;
    public Transform location;
    public Vector3 offset;
    public Vector3 dimensions;
    public float fontSize = 1.1f;
    public float fadeInDuration;
    public float fadeOutDuration;
    public float fadeTimeLeft;
    public bool fading = false;
    
    TMPro.TextMeshPro text;
    GameObject body;
    RectTransform textTransform;

    IEnumerator lifeCoroutine;

    public MovementMode mode;

    public enum MovementMode
    {
        Exact,
        Soft
    }

    public void Start()
    {
        text = transform.Find("Text").GetComponent<TMPro.TextMeshPro>();
        body = transform.Find("Body").gameObject;
        textTransform = text.GetComponent<RectTransform>();

        text.fontSize = fontSize;

        transform.SetParent(null);
        transform.position = location.position + location.TransformVector(offset);
        transform.rotation = location.rotation;

        lifeCoroutine = fadeInFadeOutDie();
        StartCoroutine(lifeCoroutine);
    }

    void Update()
    {
        if (mode == MovementMode.Exact)
        {
            transform.position = location.position + location.TransformVector(offset);
        }
        else if (mode == MovementMode.Soft)
        {
            var exactLocation = (location.position + location.TransformVector(offset));
            var actualLocation = transform.position;
            var difference = exactLocation - actualLocation;
            transform.position += difference / 10f;


            var newRotation = Quaternion.Lerp(location.rotation, location.rotation*transform.rotation, 1f);
            transform.rotation.Set(newRotation.x, newRotation.y, newRotation.z, newRotation.w);
        }

        var scale = location.localScale;
        scale.Scale(dimensions);
        body.transform.localScale = scale;
        textTransform.sizeDelta = scale;

        transform.rotation = location.rotation;
    }

    public void resize(Vector3 dim)
    {
        dimensions = new Vector3(dim.x, dim.y, 1f);
    }

    public void setText(string newText, float fontSize = 2f, TMPro.TextAlignmentOptions alignment = TMPro.TextAlignmentOptions.Left)
    {
        text.text = newText;
        text.fontSize = fontSize;
        text.alignment = alignment;
    }

    public void setFontSize(float size)
    {
        text.fontSize = size;
    }

    public IEnumerator fadeInFadeOutDie()
    {
        fadeIn();
        yield return new WaitForSeconds(lifetime);
        fadeOut((obj) => { Destroy(gameObject); });
    }

    public void fadeEarly(float fadeDuration, Async.EventDelegate onDone = null)
    {
        if (lifeCoroutine != null)
        {
            fadeOutDuration = fadeDuration;
            StopCoroutine(lifeCoroutine);
            lifeCoroutine = null;
        }

        fadeOut(onDone);
    }

    public void fadeIn(Async.EventDelegate onDone = null)
    {
        fadeAll(0f, 1f, fadeInDuration, onDone);
    }

    public void fadeOut(Async.EventDelegate onDone = null)
    {
        fadeAll(1f, 0f, fadeOutDuration, onDone);
    }

    public void fadeAll(float start, float end, float duration, Async.EventDelegate onDone = null)
    {
        foreach (Transform child in transform)
        {
            StartCoroutine(fade(child.gameObject, start, end, duration, onDone));
        }
    }

    public IEnumerator fade(GameObject child, float start, float end, float duration, Async.EventDelegate onDone = null)
    {
        fading = true;
        
        Material mat = child.GetComponent<Renderer>().material;
        string colorName = "_Color";
        Color col = Color.white;

        if (mat.HasProperty("_FaceColor")) colorName = "_FaceColor";
        if (mat.HasProperty(colorName)) col = mat.GetColor(colorName);

        for (float i = 0.0f; i < 1.0f; i += Time.deltaTime * (1 / duration))
        {
            col = mat.GetColor(colorName);
            col.a = Mathf.Lerp(start, end, i);
            child.GetComponent<Renderer>().material.SetColor(colorName, col);
            yield return null;
        }

        col.a = end;
        child.GetComponent<Renderer>().material.SetColor(colorName, col);
        onDone?.Invoke(null);

        fading = false;
    }
}
