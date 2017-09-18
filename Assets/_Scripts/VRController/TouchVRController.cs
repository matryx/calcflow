using UnityEngine;
using System.Collections;
using System;
using System.Collections.Generic;

namespace NanoVRController
{
    public class TouchVRController : VRController
    {
        private OVRInput.Controller ovrController = OVRInput.Controller.None;
        //private OvrTouch.Hands.VelocityTracker velocityTracker;

        protected override void Awake()
        {
            InitializeController();

            components = new Dictionary<ButtonId, ControllerComponent>();
            ControllerComponent c = new ControllerComponent(this);

            // Oculus Home Button
            //c = new ControllerComponent(this);
            //components.Add(ButtonId.HOME, c);

            // Grip Trigger
            c = new ControllerComponent(this);
            components.Add(ButtonId.GRIP, c);

            // Trigger
            c = new ControllerComponent(this);
            components.Add(ButtonId.TRIGGER, c);

            // Thumb Stick
            c = new ControllerComponent(this);
            components.Add(ButtonId.THUMBPAD, c);

            // A/X Button
            c = new ControllerComponent(this);
            components.Add(ButtonId.BUTTON1, c);

            // B/Y Button
            c = new ControllerComponent(this);
            components.Add(ButtonId.BUTTON2, c);

            // Thumb Rest
            c = new ControllerComponent(this);
            components.Add(ButtonId.THUMBREST, c);
        }
        protected override void Update()
        {
            // A/X Button
            if(OVRInput.Get(OVRInput.Button.One, ovrController))
            {
                components[ButtonId.BUTTON1].press = true;
            }
            // B/Y Button
            if(OVRInput.Get(OVRInput.Button.Two, ovrController))
            {
                components[ButtonId.BUTTON2].press = true;
            }
            // Trigger
            if(OVRInput.Get(OVRInput.Touch.PrimaryIndexTrigger, ovrController))
            {
                components[ButtonId.TRIGGER].axis1 = OVRInput.Get(OVRInput.Axis1D.PrimaryIndexTrigger, ovrController);
                components[ButtonId.TRIGGER].touch = true;
            }
            if(OVRInput.Get(OVRInput.Button.PrimaryIndexTrigger, ovrController))
            {
                components[ButtonId.TRIGGER].axis1 = OVRInput.Get(OVRInput.Axis1D.PrimaryIndexTrigger, ovrController);
                components[ButtonId.TRIGGER].press = true;
            }
            // Grip Trigger
            if (OVRInput.Get(OVRInput.Button.PrimaryHandTrigger, ovrController))
            {
                components[ButtonId.GRIP].axis1 = OVRInput.Get(OVRInput.Axis1D.PrimaryHandTrigger, ovrController);
                components[ButtonId.GRIP].press = true;
            }
            // Thumb Stick
            if (OVRInput.Get(OVRInput.Touch.PrimaryThumbstick, ovrController))
            {
                components[ButtonId.THUMBPAD].axis1 = OVRInput.Get(OVRInput.Axis2D.PrimaryThumbstick, ovrController).x;
                components[ButtonId.THUMBPAD].axis2 = OVRInput.Get(OVRInput.Axis2D.PrimaryThumbstick, ovrController).y;
                components[ButtonId.THUMBPAD].touch = true;
            }
            if(OVRInput.Get(OVRInput.Button.PrimaryThumbstick, ovrController))
            {
                components[ButtonId.THUMBPAD].axis1 = OVRInput.Get(OVRInput.Axis2D.PrimaryThumbstick, ovrController).x;
                components[ButtonId.THUMBPAD].axis2 = OVRInput.Get(OVRInput.Axis2D.PrimaryThumbstick, ovrController).y;
                components[ButtonId.THUMBPAD].press = true;
            }
            // Thumb Rest
            if(OVRInput.Get(OVRInput.Touch.PrimaryThumbRest, ovrController))
            {
                components[ButtonId.THUMBREST].touch = true;
            }

            velocity = OVRInput.GetLocalControllerVelocity(ovrController);
            //angularVelocity = OVRInput.GetLocalControllerAngularVelocity(ovrController).eulerAngles;
            position = OVRInput.GetLocalControllerPosition(ovrController);
            rotation = OVRInput.GetLocalControllerRotation(ovrController);

            foreach (ControllerComponent c in components.Values)
            {
                c.UpdateComponent();
            }

            float elapsed = Time.time - hapticStartTime;
            if(elapsed >= hapticDuration)
            {
                OVRInput.SetControllerVibration(0f, 0f, ovrController);
            }
        }

        private void InitializeController()
        {
            ovrController = (hand == Handedness.LEFT) ? OVRInput.Controller.LTouch : OVRInput.Controller.RTouch;
        }

        public override void SendHapticEvent(float frequency, float amplitude, float duration)
        {
            hapticStartTime = Time.time;
            hapticDuration = duration;

            OVRInput.SetControllerVibration(frequency, amplitude, ovrController);
        }
    }
}
