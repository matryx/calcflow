using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class WaitAndThen : MonoBehaviour
{

    public float WaitForSeconds = 0f;
    public float secondsRemaining;

    public List<MonoBehaviour> todo = new List<MonoBehaviour>();
    void Start()
    {
        secondsRemaining = WaitForSeconds;
    }

    // Update is called once per frame
    void Update()
    {
        secondsRemaining -= Time.deltaTime;

        if (secondsRemaining <= 0.0f)
        {
            timerEnded();
            Destroy(this);
        }

    }

    void timerEnded()
    {
        foreach (MonoBehaviour Mobo in todo)
        {
			Mobo.enabled = true;
        }
    }
}
