using UnityEngine;
using UnityEngine.UI;

namespace Crosstales.UI
{
    /// <summary>Change the Focus on from a Window.</summary>
    public class UIFocus : MonoBehaviour
    {
        #region Variables

        /// <summary>Name of the gameobject containing the UIWindowManager.</summary>
        [Tooltip("Name of the gameobject containing the UIWindowManager.")]
        public string ManagerName = "Canvas";

        private UIWindowManager manager;
        private Image image;

        private Transform tf;

        #endregion


        #region MonoBehaviour methods

        public void Start()
        {
            tf = transform;

            manager = GameObject.Find(ManagerName).GetComponent<UIWindowManager>();

            image = tf.Find("Panel/Header").GetComponent<Image>();
        }

        #endregion


        #region Public methods

        ///<summary>Panel entered.</summary>
        public void OnPanelEnter()
        {
            manager.ChangeState(gameObject);

            Color c = image.color;
            c.a = 255;
            image.color = c;

            tf.SetAsLastSibling(); //move to the front (on parent)
            tf.SetAsFirstSibling(); //move to the back (on parent)
            tf.SetSiblingIndex(-1); //move to position, whereas 0 is the backmost, transform.parent.childCount -1 is the frontmost position 
            tf.GetSiblingIndex(); //get the position in the hierarchy (on parent)
        }

        #endregion
    }
}
// © 2017-2019 crosstales LLC (https://www.crosstales.com)