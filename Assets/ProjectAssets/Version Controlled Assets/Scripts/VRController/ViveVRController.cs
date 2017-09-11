using UnityEngine;
using System.Collections;
using System;
using System.Collections.Generic;

namespace NanoVRController
{
    /// <summary>
    /// Class that defines the HTC Vive controller and
    /// wraps input into events
    /// </summary>
    [RequireComponent(typeof(SteamVR_TrackedObject))]
    public class ViveVRController : VRController
    {
        SteamVR_TrackedObject trackedObj;
        ushort hapticAmp;

        protected override void Awake()
        {
            trackedObj = GetComponent<SteamVR_TrackedObject>();

            components = new Dictionary<ButtonId, ControllerComponent>();
            ControllerComponent c = new ControllerComponent(this);

            // Application Menu Button
            c = new ControllerComponent(this);
            components.Add(ButtonId.MENU, c);

            // Steam Home Button
            c = new ControllerComponent(this);
            components.Add(ButtonId.HOME, c);

            // Grip Button
            c = new ControllerComponent(this);
            components.Add(ButtonId.GRIP, c);

            // Trigger
            c = new ControllerComponent(this);
            components.Add(ButtonId.TRIGGER, c);

            // Touchpad
            c = new ControllerComponent(this);
            components.Add(ButtonId.THUMBPAD, c);
        }

        private void switchMode()
        {
            pokeMode = !pokeMode;
        }

        bool lastgrip = false;

        protected override void Update()
        {
            var device = SteamVR_Controller.Input((int)trackedObj.index);
            if (device.GetPress(SteamVR_Controller.ButtonMask.ApplicationMenu))
            {
                components[ButtonId.MENU].press = true;
            }
            if (device.GetPress(SteamVR_Controller.ButtonMask.System))
            {
                components[ButtonId.HOME].press = true;
            }
            if (device.GetPress(SteamVR_Controller.ButtonMask.Grip))
            {
                if (lastgrip == false)
                {
                    switchMode();
                }
                lastgrip = true;
            }
            else
            {
                lastgrip = false;
            }
            if (device.GetPress(SteamVR_Controller.ButtonMask.Trigger))
            {
                if (pokeMode)
                {
                    components[ButtonId.TRIGGER].axis1 = device.GetAxis(Valve.VR.EVRButtonId.k_EButton_SteamVR_Trigger).x;
                    components[ButtonId.TRIGGER].press = true;
                } else
                {
                    components[ButtonId.GRIP].axis1 = device.GetAxis(Valve.VR.EVRButtonId.k_EButton_SteamVR_Trigger).x;
                    components[ButtonId.GRIP].press = true;
                }

            }
            if (device.GetTouch(SteamVR_Controller.ButtonMask.Touchpad))
            {
                components[ButtonId.THUMBPAD].axis1 = device.GetAxis(Valve.VR.EVRButtonId.k_EButton_SteamVR_Touchpad).x;
                components[ButtonId.THUMBPAD].axis2 = device.GetAxis(Valve.VR.EVRButtonId.k_EButton_SteamVR_Touchpad).y;
                components[ButtonId.THUMBPAD].touch = true;
            }
            if (device.GetPress(SteamVR_Controller.ButtonMask.Touchpad))
            {
                components[ButtonId.THUMBPAD].axis1 = device.GetAxis(Valve.VR.EVRButtonId.k_EButton_SteamVR_Touchpad).x;
                components[ButtonId.THUMBPAD].axis2 = device.GetAxis(Valve.VR.EVRButtonId.k_EButton_SteamVR_Touchpad).y;
                components[ButtonId.THUMBPAD].press = true;
            }

            position = device.transform.pos;
            rotation = device.transform.rot;
            velocity = device.velocity;
            angularVelocity = device.angularVelocity;

            foreach(ControllerComponent c in components.Values)
            {
                c.UpdateComponent();
            }

            float elapsed = Time.time - hapticStartTime;
            if(elapsed <= hapticDuration)
            {
                device.TriggerHapticPulse(hapticAmp);
            }
        }

        public override void SendHapticEvent(float frequency, float amplitude, float duration)
        {
            hapticStartTime = Time.time;
            hapticDuration = duration;
            hapticAmp = (ushort)(amplitude * 3999);
            var device = SteamVR_Controller.Input((int)trackedObj.index);
            device.TriggerHapticPulse(hapticAmp);
        }
    }
}
