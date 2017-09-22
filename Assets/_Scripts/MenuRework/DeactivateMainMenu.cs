using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class DeactivateMainMenu : QuickButton {
    public Transform toggleButton;
    private Vector3 idleScale;

    // Use this for initialization
    protected override void Start()
    {
        base.Start();
        idleScale = new Vector3(0.02f, 0.001f, 0.02f);
	}
	
	// Update is called once per frame
	void Update () {
		
	}

    IEnumerator ScaleTo(Transform obj, Vector3 start, Vector3 end, float overTime)
    {
        float startTime = Time.time;

        while (Time.time < startTime + overTime)
        {
            obj.localScale = Vector3.Lerp(start, end, (Time.time - startTime) / overTime);
            yield return null;
        }

        obj.localScale = end;
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
        StartCoroutine(ScaleTo(toggleButton, toggleButton.localScale, idleScale, 0.1f));
    }
}
