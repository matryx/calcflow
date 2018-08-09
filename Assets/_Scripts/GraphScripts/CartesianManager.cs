using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class CartesianManager : MonoBehaviour
{
    public static CartesianManager _instance;
    void Awake(){
        _instance = this;
    }

    [SerializeField]
    public AxisLabelManager xLabel;
    [SerializeField]
    public AxisLabelManager yLabel;
    [SerializeField]
    public AxisLabelManager zLabel;

    private float range = 10;
    private void UpdateAxis(float newRange)
    {
        xLabel.Max = range; xLabel.Min = -range;
        yLabel.Max = range; yLabel.Min = -range;
        zLabel.Max = range; zLabel.Min = -range;
    }

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
            UpdateAxis(range);
        }

    }
}
