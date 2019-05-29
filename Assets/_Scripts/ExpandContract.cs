using Nanome.Core;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ExpandContract : MonoBehaviour {

	Vector3 savedPos, savedScale;
	Quaternion savedRot;

	void Start () {
		savedPos = new Vector3();
		savedScale = new Vector3(1f, 1f, 1f);
		savedRot = new Quaternion();
	}

	public IEnumerator Contract(float time = 0.3f, Async.EventDelegate afterContracted = null)
    {
		savedPos = transform.localPosition;
		savedScale = transform.localScale;
		savedRot = transform.localRotation;
		yield return StartCoroutine(ScaleTo(transform, transform.localScale, Vector3.zero, time, afterContracted));
        transform.gameObject.SetActive(false);
	}

	public IEnumerator Expand(float time = 0.3f, Async.EventDelegate afterExpanded = null){
		transform.gameObject.SetActive(true);
		yield return StartCoroutine(ScaleTo(transform, Vector3.zero, savedScale, time, afterExpanded));
        afterExpanded(this);
	}
	
	IEnumerator ScaleTo(Transform obj, Vector3 start, Vector3 end, float overTime, Async.EventDelegate whenDone = null)
    {
        float startTime = Time.time;

        while (Time.time < startTime + overTime)
        {
            if (obj == null)
            {
                yield break;
            }
            obj.localScale = Vector3.Lerp(start, end, (Time.time - startTime) / overTime);
            yield return null;
        }

        obj.localScale = end;
        whenDone?.Invoke(this);
    }
}
