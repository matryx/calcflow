using UnityEngine;

namespace Crosstales.UI
{
    /// <summary>Manager for a Window.</summary>
    public class WindowManager : MonoBehaviour
    {
        #region Variables

        /// <summary>Window movement speed (default: 3).</summary>
        [Tooltip("Window movement speed (default: 3).")]
        public float Speed = 3f;

        /// <summary>Dependent GameObjects (active == open).</summary>
        [Tooltip("Dependent GameObjects (active == open).")]
        public GameObject[] Dependencies;

        private UIFocus focus;

        private bool open;
        private bool close;

        private Vector3 startPos;
        private Vector3 centerPos;
        private Vector3 lerpPos;

        private float openProgress;
        private float closeProgress;

        private GameObject panel;

        private Transform tf;

        #endregion


        #region MonoBehaviour methods

        public void Start()
        {
            tf = transform;

            panel = tf.Find("Panel").gameObject;

            startPos = tf.position;

            ClosePanel();

            panel.SetActive(false);

            if (Dependencies != null)
            {
                foreach (GameObject go in Dependencies)
                {
                    go.SetActive(false);
                }
            }
        }

        public void Update()
        {
            centerPos = new Vector3(Screen.width / 2, Screen.height / 2, 0);

            if (open && openProgress < 1f)
            {
                openProgress += Speed * Time.deltaTime;

                tf.position = Vector3.Lerp(lerpPos, centerPos, openProgress);
            }
            else if (close)
            {
                if (closeProgress < 1f)
                {
                    closeProgress += Speed * Time.deltaTime;

                    tf.position = Vector3.Lerp(lerpPos, startPos, closeProgress);
                }
                else
                {
                    panel.SetActive(false);

                    if (Dependencies != null)
                    {
                        foreach (GameObject go in Dependencies)
                        {
                            go.SetActive(false);
                        }
                    }
                }
            }
        }

        #endregion


        #region Public methods

        ///<summary>Switch between open and close.</summary>
        public void SwitchPanel()
        {
            if (open)
            {
                ClosePanel();
            }
            else
            {
                OpenPanel();
            }
        }

        ///<summary>Open the panel.</summary>
        public void OpenPanel()
        {
            panel.SetActive(true);

            if (Dependencies != null)
            {
                foreach (GameObject go in Dependencies)
                {
                    go.SetActive(true);
                }
            }

            focus = gameObject.GetComponent<UIFocus>();
            focus.OnPanelEnter();

            lerpPos = tf.position;
            open = true;
            close = false;
            openProgress = 0f;
        }

        ///<summary>Close the panel.</summary>
        public void ClosePanel()
        {
            lerpPos = tf.position;
            open = false;
            close = true;
            closeProgress = 0f;
        }

        #endregion
    }
}
// © 2017-2019 crosstales LLC (https://www.crosstales.com)