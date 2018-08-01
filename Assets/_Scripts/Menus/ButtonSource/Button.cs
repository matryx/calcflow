using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Calcflow.UserStatistics;

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

            string eventName = gameObject.name;
            var extra = new Dictionary<string, object>();

            extra["parent"] = (gameObject.transform.parent != null) ? gameObject.transform.parent.name : null;
            if (!eventName.Equals("Body"))
            {
                StatisticsTracking.StartEvent("Button Press", eventName, extra);
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
            if (OnButtonExit != null)
                OnButtonExit.Invoke(other);

            string eventName = gameObject.name;
            if (!eventName.Equals("Body"))
            {
                StatisticsTracking.EndEvent("Button Press", eventName);
            }
        }

#if UNITY_EDITOR
        public bool verbose = false;
        public bool editorPress = false;
        bool editorPressed = false;
#endif
        protected virtual void Update()
        {
#if UNITY_EDITOR
            if (!editorPressed && editorPress)
            {
                editorPressed = editorPress;
                PressButton(this.gameObject);
            }
            else if (editorPressed && !editorPress)
            {
                editorPressed = editorPress;
                UnpressButton(this.gameObject);
            }
#endif
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
    }
}
