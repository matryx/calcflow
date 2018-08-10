using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class AxisLabelManager : MonoBehaviour
{

    private float min = -10;
    private float max = 10;

    public bool skipFirst = false;
    public bool skipLast = false;

    public TextMesh[] labels;

    public float Min
    {
        get
        {
            return min;
        }
        set
        {
            if (value != min)
            {
                min = value;
                updateLabels();
            }
        }
    }

    public float Max
    {
        get
        {
            return max;
        }
        set
        {
            if (value != max)
            {
                max = value;
                updateLabels();
            }
        }
    }

    private void updateRange(float newRange)
    {
        Min = -newRange;
        Max = newRange;
    }

    private void updateLabels()
    {
        int numLabels = labels.Length - 1;
        numLabels += skipFirst ? 1 : 0;
        numLabels += skipLast ? 1 : 0;
        int i = skipFirst ? 1 : 0;
        float increment = (max - min) / numLabels;
        foreach (TextMesh l in labels)
        {
            l.text = string.Format("{0:F3}", min + (i * increment));
            i++;
        }
    }

    // Use this for initialization
    void Start()
    {
        CartesianManager._instance.AddScaleCallback(updateRange);
    }
    void OnDestroy()
    {
        CartesianManager._instance.RemoveScaleCallback(updateRange);
    }

    // Update is called once per frame
    void Update()
    {

    }


}
