using UnityEngine;
using System.Collections;
using System.Collections.Generic;

namespace NanoVRController
{
    public struct ControllerComponentArgs
    {
        public float x;
        public float y;
    }

    public delegate void ControllerComponentCallback(VRController c, ControllerComponentArgs e);

    /// <summary>
    /// Class that defines an interactable component of the 
    /// controller and raises coresponding events
    /// </summary>
    public class ControllerComponent
    {
        public VRController controller;
        protected bool isPressed;
        public bool press;
        protected bool isTouched;
        public bool touch;
        public float axis1;
        public float axis2;

        public event ControllerComponentCallback ComponentTouched;
        public event ControllerComponentCallback ComponentTouching;
        public event ControllerComponentCallback ComponentReleased;
        public event ControllerComponentCallback ComponentPressed;
        public event ControllerComponentCallback ComponentPressing;
        public event ControllerComponentCallback ComponentUnpressed;

        public ControllerComponent(VRController c)
        {
            controller = c;
        }

        protected virtual void OnTouch(ControllerComponentArgs e)
        {
            if (ComponentTouched != null)
                ComponentTouched.Invoke(controller, e);
        }
        protected virtual void OnTouching(ControllerComponentArgs e)
        {
            if (ComponentTouching != null)
                ComponentTouching.Invoke(controller, e);
        }
        protected virtual void OnRelease(ControllerComponentArgs e)
        {
            if (ComponentReleased != null)
                ComponentReleased.Invoke(controller, e);
        }
        protected virtual void OnPress(ControllerComponentArgs e)
        {
            if (ComponentPressed != null)
                ComponentPressed.Invoke(controller, e);
        }
        protected virtual void OnPressing(ControllerComponentArgs e)
        {
            if (ComponentPressing != null)
                ComponentPressing.Invoke(controller, e);
        }
        protected virtual void OnUnpressed(ControllerComponentArgs e)
        {
            if (ComponentUnpressed != null)
                ComponentUnpressed.Invoke(controller, e);
        }

        public void UpdateComponent()
        {
            if (!isPressed && press)
            {
                isPressed = true;
                ControllerComponentArgs e;
                e.x = axis1;
                e.y = axis2;
                OnPress(e);
            }
            else if (isPressed && press)
            {
                isPressed = true;
                ControllerComponentArgs e;
                e.x = axis1;
                e.y = axis2;
                OnPressing(e);
            }
            else if (isPressed && !press)
            {
                isPressed = false;
                ControllerComponentArgs e;
                e.x = axis1;
                e.y = axis2;
                OnUnpressed(e);
            }

            if (!isTouched && touch)
            {
                isTouched = true;
                ControllerComponentArgs e;
                e.x = axis1;
                e.y = axis2;
                OnTouch(e);
            }
            else if (isTouched && touch)
            {
                isTouched = true;
                ControllerComponentArgs e;
                e.x = axis1;
                e.y = axis2;
                OnTouching(e);
            }
            else if (isTouched && !touch)
            {
                isTouched = false;
                ControllerComponentArgs e;
                e.x = axis1;
                e.y = axis2;
                OnRelease(e);
            }

            // reset for next frame
            press = touch = false;
        }
    }
}
