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

	public IEnumerator Contract(float time = 0.3f){
		savedPos = transform.localPosition;
		savedScale = transform.localScale;
		savedRot = transform.localRotation;
		yield return StartCoroutine(ScaleTo(transform, transform.localScale, Vector3.zero, time));
        transform.gameObject.SetActive(false);
	}

	public IEnumerator Expand(float time = 0.3f){
		transform.gameObject.SetActive(true);
		yield return StartCoroutine(ScaleTo(transform, Vector3.zero, savedScale, time));
	}
	
	IEnumerator ScaleTo(Transform obj, Vector3 start, Vector3 end, float overTime)
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
    }
}
