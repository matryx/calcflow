using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace CalcFlowUI
{
    public class Button : MonoBehaviour
    {
        public delegate void ButtonCallBack(GameObject presser);

        public event ButtonCallBack OnButtonEnter;
        public event ButtonCallBack OnButtonExit;

        [SerializeField]
        public Color disabledColor;

        public virtual void PressButton(GameObject other)
        {
            #if UNITY_EDITOR
            if (verbose)
            {
                print("button pressed");
            }
            #endif
            if (OnButtonEnter != null)
                OnButtonEnter.Invoke(other);
        }

        public virtual void UnpressButton(GameObject other)
        {
            #if UNITY_EDITOR
            if (verbose)
            {
                print("button released");
            }
            #endif
            if (OnButtonExit != null)
                OnButtonExit.Invoke(other);
        }

        #if UNITY_EDITOR
        public bool verbose = false;
        public bool press = false;
        bool pressed = false;
        bool Pressed
        {
            get
            {
                return pressed;
            }
            set
            {
                if (!pressed && value)
                {
                    pressed = value;
                    PressButton(this.gameObject);
                }
                else if (pressed && !value)
                {
                    pressed = value;
                    UnpressButton(this.gameObject);
                }
            }
        }

        public void Disable()
        {
            GetComponent<Renderer>().material.color = disabledColor;
            foreach (ButtonCallBack b in OnButtonEnter.GetInvocationList())
            {
                OnButtonEnter -= b;
            }

            HighlightOnRaycast highlight = GetComponent<HighlightOnRaycast>();
            if (highlight != null)
            {
                Destroy(highlight);
            }
        }

        public void Update()
        {
            Pressed = press;
        }
        #endif
    }
}