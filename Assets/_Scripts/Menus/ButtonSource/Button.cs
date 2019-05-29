using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Calcflow.UserStatistics;

namespace CalcFlowUI
{
    public class Button : MonoBehaviour
    {
        public delegate void ButtonCallBack(GameObject presser);

        public event ButtonCallBack OnButtonStay;
        public event ButtonCallBack OnButtonPress;
        public event ButtonCallBack OnButtonUnpress;
        public event ButtonCallBack OnButtonLeave;

        [SerializeField]
        public Color disabledColor;
        public bool verbose = false;

        public virtual void PressButton(GameObject other)
        {

#if UNITY_EDITOR
            if (verbose)
            {
                print("button pressed");
            }
#endif
            if (OnButtonPress != null)
                OnButtonPress.Invoke(other);

            string eventName = gameObject.name;
            var extra = new Dictionary<string, object>();
            extra["parent"] = gameObject.transform.parent.name;
            if (!eventName.Equals("Body"))
            {
                StatisticsTracking.StartEvent("Button Press", eventName, extra);
            }
        }

        public virtual void HoverButton(GameObject other)
        {
            if (verbose)
            {
                print("button hovering");
            }

            OnButtonStay?.Invoke(other);
        }

        public virtual void LeaveButton(GameObject other)
        {
            if (verbose)
            {
                print("button unhovered");

                OnButtonLeave?.Invoke(other);
            }
        }

        public virtual void UnpressButton(GameObject other)
        {
#if UNITY_EDITOR
            if (verbose)
            {
                print("button released");
            }
#endif
            if (OnButtonUnpress != null)
                OnButtonUnpress.Invoke(other);

            string eventName = gameObject.name;
            if (!eventName.Equals("Body"))
            {
                StatisticsTracking.EndEvent("Button Press", eventName);
            }
        }

#if UNITY_EDITOR
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
#endif
        protected virtual void Update()
        {
#if UNITY_EDITOR
            Pressed = press;
#endif
        }


        public void Disable()
        {
            GetComponent<Renderer>().material.color = disabledColor;
            foreach (ButtonCallBack b in OnButtonPress.GetInvocationList())
            {
                OnButtonPress -= b;
            }

            HighlightOnRaycast highlight = GetComponent<HighlightOnRaycast>();
            if (highlight != null)
            {
                Destroy(highlight);
            }
        }
    }
}
