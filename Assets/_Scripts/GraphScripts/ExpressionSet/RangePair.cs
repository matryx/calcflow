using System.Collections;
using System.Collections.Generic;
using UnityEngine;


[System.Serializable]
public class RangePair
{
    public Range Min;
    public Range Max;

    public RangePair(Range min, Range max)
    {
        Min = min;
        Max = max;
    }

    public RangePair(RangePair toCopy)
    {
        Min = new Range(toCopy.Min);
        Max = new Range(toCopy.Max);
    }
}