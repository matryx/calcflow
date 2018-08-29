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
      Application.OpenURL("http://nanome.ai/nanome");
      StopAllCoroutines();
      StartCoroutine(Darken(feedback, .5f, .3f, 1f));
    }

	protected override void ButtonExitBehavior(GameObject other)
    {

    }

    IEnumerator Darken(Transform obj, float animTime, float animFade, float waitDuration){
        float startTime = Time.time;
        while (Time.time < startTime + animTime)
        {
            if (obj == null)
            {
                yield break;
            }
            float a = Mathf.Lerp(0f, 1f, (Time.time - startTime) / animTime);
            obj.GetComponent<SpriteRenderer>().color = new Color(1f,1f,1f,a);
            yield return null;
        }
        startTime = Time.time;
        while (Time.time < startTime + waitDuration){
            yield return null;
        }
        startTime = Time.time;
        while (Time.time < startTime + animFade)
        {
            if (obj == null)
            {
                yield break;
            }
            float a = Mathf.Lerp(1f, 0f, (Time.time - startTime) / animFade);
            obj.GetComponent<SpriteRenderer>().color = new Color(1f,1f,1f,a);
            yield return null;
        }
        obj.GetComponent<SpriteRenderer>().color = new Color(1f,1f,1f,0f);
    }

}
