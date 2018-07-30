using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class PBT_MoveInCircles : MonoBehaviour
{

    public float radius = 5.0f;
    public float speed = 5.0f;

    private float degree = 0.0f;
    private Vector3 center;
    void Awake()
    {
        center = transform.position;
        transform.position = center + new Vector3(1, 0, 0) * radius;
    }

    // Update is called once per frame
    void Update()
    {
        if (Replayer.Replaying)
        {
            Destroy(this);
        }
        degree += speed;
        transform.position = center + radius * new Vector3(Mathf.Cos(degree), 0, Mathf.Sin(degree));
    }
}
