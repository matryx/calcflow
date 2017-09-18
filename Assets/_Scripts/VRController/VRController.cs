using UnityEngine;
using System.Collections;
using System;
using System.Collections.Generic;

namespace NanoVRController
{
    public enum ButtonId
    {
        MENU,
        HOME,
        GRIP,
        TRIGGER,
        THUMBPAD,
        BUTTON1,
        BUTTON2,
        THUMBREST
    }

    public enum Handedness
    {
        LEFT,
        RIGHT
    }

    /// <summary>
    /// Abstract class that defines controller api
    /// </summary>
    public abstract class VRController : MonoBehaviour
    {
        //mode needed for vive legacy support.
        public bool pokeMode = true;

        public Dictionary<ButtonId, ControllerComponent> components;

        protected Vector3 position;
        protected Quaternion rotation;
        protected Vector3 velocity;
        protected Vector3 angularVelocity;

        protected float hapticStartTime = -1f;
        protected float hapticDuration = -1f;

        public Vector3 Position { get { return position; } }
        public Quaternion Rotation { get { return rotation; } }
        public Vector3 Velocity { get { return velocity; } }
        public Vector3 AngularVelocity { get { return angularVelocity; } }

        [SerializeField]
        public Transform origin;
        [SerializeField]
        public Handedness hand;

        protected abstract void Awake();

        protected abstract void Update();

        public abstract void SendHapticEvent(float frequency, float amplitude, float duration);
    }
}
