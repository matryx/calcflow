using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using VoxelBusters.RuntimeSerialization;

[RuntimeSerializable(typeof(MonoBehaviour), true, false)]
public class ColorTester : MonoBehaviour
{
    public ColorTester() {}

    public Color color;
    public int integer;

    // Use this for initialization
    void Start()
    {

    }

    // Update is called once per frame
    void Update()
    {
        if (color.r == 255)
        {
            print("color not serializing correctly;");
        }
        if (integer != 32) {
            print("integer not serializing correctly;");
        }
    }
}
