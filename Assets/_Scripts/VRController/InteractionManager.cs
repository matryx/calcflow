using UnityEngine;
using System.Collections;

namespace NanoVRController
{
    public abstract class InteractionManager : MonoBehaviour
    {
        [SerializeField]
        protected VRController left, right;

        protected abstract void Start();
        protected abstract void Update();
    }
}