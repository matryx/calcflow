using UnityEngine;
using System.Collections;
using System;
using UnityEngine.SceneManagement;
using CalcFlowUI;

public class WebRedirect : QuickButton {

    public Transform feedback;

    private void Update()
    {

    }
    protected override void ButtonEnterBehavior(GameObject other)
    {
        transform.GetComponent<AudioSource>().Play();
        Application.OpenURL("http://nanome.ai/nanome");
        // Application.OpenURL("http://nanome.ai/pricing");
        StopAllCoroutines();
        StartCoroutine(FadeInOut(feedback, .5f, .3f, 1f));
    }

	protected override void ButtonExitBehavior(GameObject other)
    {

    }

    IEnumerator FadeInOut(Transform obj, float fadeInTime, float fadeOutTime, float waitDuration){
        float startTime = Time.time;
        while (Time.time < startTime + fadeInTime)
        {
            if (obj == null)
            {
                yield break;
            }
            float a = Mathf.Lerp(0f, 1f, (Time.time - startTime) / fadeInTime);
            obj.GetComponent<SpriteRenderer>().color = new Color(1f,1f,1f,a);
            yield return null;
        }
        startTime = Time.time;
        while (Time.time < startTime + waitDuration){
            yield return null;
        }
        startTime = Time.time;
        while (Time.time < startTime + fadeOutTime)
        {
            if (obj == null)
            {
                yield break;
            }
            float a = Mathf.Lerp(1f, 0f, (Time.time - startTime) / fadeOutTime);
            obj.GetComponent<SpriteRenderer>().color = new Color(1f,1f,1f,a);
            yield return null;
        }
        obj.GetComponent<SpriteRenderer>().color = new Color(1f,1f,1f,0f);
    }

}
