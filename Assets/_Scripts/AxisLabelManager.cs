using System.Collections;
using System.Collections.Generic;
using TMPro;
using UnityEngine;
using VoxelBusters.RuntimeSerialization;

[RuntimeSerializable(typeof(MonoBehaviour), true, true)]
public class AxisLabelManager : MonoBehaviour {

    private float min = -10;
    private float max = 10;

    public bool skipFirst = false;
    public bool skipLast = false;

    public TextMeshPro[] labels;

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

    private void updateLabels()
    {
        int numLabels = labels.Length - 1;
        numLabels += skipFirst ? 1 : 0;
        numLabels += skipLast ? 1 : 0;
        int i = skipFirst ? 1 : 0;
        float increment = (max - min) / numLabels;
        foreach (TextMeshPro l in labels)
        {
            if (l==null) print (this.transform.GetChild(0).name);
            l.text = string.Format("{0:F3}", min + (i * increment));
            i++;
        }
    }    

    // Use this for initialization
    void Start () {
		
	}
	
	// Update is called once per frame
	void Update () {
		
	}


}
