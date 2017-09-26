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

        public virtual void PressButton(GameObject other)
        {
            if(OnButtonEnter != null)
                OnButtonEnter.Invoke(other);
        }

        public virtual void UnpressButton(GameObject other)
        {
            if (OnButtonExit != null)
                OnButtonExit.Invoke(other);
        }
    }
}
