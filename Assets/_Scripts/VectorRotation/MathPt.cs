using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace VectorRotation
{
    public class MathPt : MonoBehaviour
    {
        float actualX;
        float actualY;
        float actualZ;

        float unityX;
        float unityY;
        float unityZ;

        public MathPt(float xInput, float yInput, float zInput)
        {
            actualX = xInput;
            actualY = yInput;
            actualZ = zInput;
            unityX = zInput;
            unityY = xInput;
            unityZ = yInput;
        }

        public Vector3 createVec3()
        {
            return new Vector3(unityX,unityY,unityZ);
        }

        public override string ToString()
        {
            return $"({actualX}, {actualY}, {actualZ})";
        }
    }
}
