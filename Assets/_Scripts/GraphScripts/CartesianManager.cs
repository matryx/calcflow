using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class CartesianManager : MonoBehaviour
{
    public static CartesianManager _instance;
    public delegate void ScaleChangedCallback(float newScale);
    event ScaleChangedCallback ScaleChangedEvent;
    void Awake()
    {
        _instance = this;
    }

[SerializeField]
    private float range = 10;

    public float GetScale()
    {
        return range;
    }

    public void SetScale(float newScale)
    {
        if (newScale == 0)
        {
            range = 10;
        }
        else if (newScale != range)
        {
            range = newScale;
            //UpdateAxis(range);
            if (ScaleChangedEvent != null)
            {
                ScaleChangedEvent.Invoke(newScale);
            }
        }
    }

    public void AddScaleCallback(ScaleChangedCallback callback)
    {
        callback(range);
        ScaleChangedEvent += callback;
    }

    public void RemoveScaleCallback(ScaleChangedCallback callback)
    {
        ScaleChangedEvent -= callback;
    }
}
